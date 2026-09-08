# Notes on the Vorbis decoder

Vorbis I, decoded to within float rounding of libvorbis: ten fixtures at correlation 1.000000 with
a relative RMS error under 1e-4, which in practice is a few hundredths of one LSB out of 32768.

## What is here and what is not

Floor 1, all three residue formats, square polar channel coupling, block switching with the hybrid
windows, and Ogg demux. **Floor 0** — a line spectral pair representation — is refused: no encoder
in use has emitted it since the format was frozen and libvorbis has never produced one. Refusing it
by name is better than a decoder that half-works on a file nobody has.

## The floor table is a formula

The reference implementation carries 256 printed floats for `floor1_inverse_dB_static_table`. They
are exactly `10^((i-255) * 140/5120)` — a hundred and forty decibels spread over two hundred and
fifty six steps, 0.546875 dB each, ending at unity. Generating them from that says what the table
*is*, and agrees with libvorbis's constants to 6.7e-7 relative, which is float32 rounding in the
printed values rather than disagreement.

Finding that out took extracting the table from `floor1.c` and looking at the ratio between
consecutive entries, which is constant to eight digits. A table whose neighbouring ratios are
constant is an exponential, and then it is only arithmetic to find which one.

## The inverse transform has no 4/N

The textbook inverse MDCT carries a `4/N` normalisation. The reference encoder folds exactly that
factor into its **forward** transform instead, so the coefficients in the bitstream are already
scaled and applying it again makes the output five hundred and twelve times too quiet.

The diagnostic is worth remembering because it is unambiguous: **correlation 0.999 against the
reference with a relative RMS error of 0.998**. Correlation that high means the shape is right;
relative error that close to one means the amplitude is not. Nothing but a gain error produces that
pair of numbers, and no amount of staring at the residue decode would have found it.

## Codewords are assigned in ENTRY order

The stream gives codeword lengths, not codewords; each used entry takes "the lowest valued unused
binary Huffman codeword" of its length, in entry order (§3.2.1). Entry order, not length order — a
codebook whose lengths happen to be sorted decodes correctly either way, which is exactly how this
class of bug survives into production. The same trap caught Theora's VLC reader in the sibling
repository.

Rather than the reference implementation's carry-propagating marker array, the assignment here
keeps the free subtrees of the decision tree in a list ordered left to right. Taking a codeword of
length L means taking the first free subtree no deeper than L and splitting it down to L, putting
the right-hand siblings back where it was. The list stays ordered because a subtree's descendants
all sort between it and whatever followed it — so "the first free subtree" *is* "the lowest unused
codeword" by construction rather than by argument. Both error conditions the spec names then fall
straight out: a list that empties early is an over-populated tree, one that is not empty at the end
is an under-populated one.

**Single-entry codebooks are legal and malformed at once.** A book with one used entry cannot be a
complete tree, because there is no way to write a codeword length of zero. Xiph tightened the
reference implementation in 2008, discovered streams in the wild that stopped decoding, and struck
the change from the spec in a 2015 erratum: one used entry is legal, must have been coded with
length 1, sinks one bit when read, and the value of that bit does not matter.

## The frame boundary is the window centre

Consecutive window centres are `(prev_n + n)/4` samples apart, and that is how many samples a frame
contributes. With equal block sizes that is the familiar `n/2` and the previous block's tail lines
up with the current block's head. With unequal ones the current block starts `(n - prev_n)/4`
samples before or after the previous centre — negative half the time.

Getting this wrong does not produce silence or noise. It produces audio that is wrong only where the
encoder switched block sizes, which is precisely at the transients: the hardest place to hear it and
the most annoying place to have it.

The previous block's tail never reaches past the current centre, because the window's right slope
ends at `right_window_end`, and `right_window_end - centre` works out to exactly the frame length in
every combination of long and short. That is not a coincidence to rely on quietly, so it is written
down here: it is why the lapping code can simply take the current block from its centre onward as
the next tail without carrying a remainder.

## Two conventions that are backwards from everything else here

**Bits are LSB-first.** Every other codec in reed reads MSB-first. In Vorbis the first bit of a
packet is the least significant bit of the first byte, and a multi-bit field is assembled with the
first bit read as its least significant. That is not something a byte swap fixes; it changes what
"the next four bits" means.

**Running off the end of a packet is normal.** In the headers a truncated packet is a broken stream.
In an audio packet it is the ordinary way a residue partition ends — the encoder stops writing once
the rest would be zero. A decoder that treats end-of-packet as an error refuses most real files, so
the bit reader latches a flag and returns zeros, and each caller decides which of the two situations
it is in.

## Coverage is asserted, not assumed

Short blocks, hybrid windows, channel coupling and the "unused channel" path are things a fixture
either exercises or does not, and a fixture re-encoded some day with different settings could stop
exercising one without any test failing. The suite counts them and asserts the counts are positive:
2013 short blocks, 435 hybrid windows, 3189 coupled blocks, 161 unused channels across the corpus.

## The inverse transform is one FFT of size N/4

Ten seconds of stereo decoded in about 1.8 seconds with the direct sum, and decodes in 0.09 now:
**twenty times faster**, a hundred and thirteen times real time.

The derivation is worth writing down because the constants are otherwise unguessable. Substitute
`j = i + n/4` into

    y[i] = SUM_k X[k] cos( pi (2i+1+n/2)(2k+1) / 2n )

and `2i+1+n/2` becomes `2j+1`, leaving a **DCT-IV of size M = n/2 exactly** — its denominator is
`4M` — with `y[i] = Z[i + n/4]`. The index runs past the DCT's own range at both ends, and the
kernel's two symmetries cover it: `Z[2M-1-j] = -Z[j]` and `Z[j+n] = -Z[j]`, both because
`cos(pi(2k+1)) = -1`. So the whole transform is one DCT-IV and a rearrangement with sign flips.

The DCT-IV in turn is a complex FFT of size `M/2`. Pair the input as `u[p] = x[2p] + i*x[M-1-2p]`,
which covers the evens ascending and the odds descending, and observe that

    cos(theta (2j+1)(2M-4p-1)) = (-1)^j sin(theta (2j+1)(4p+1)),    theta = pi/4M

so the cosine and sine halves of the sum are the real and imaginary parts of a single complex
product. Expanding `(4q+1)(4p+1) = 16pq + 4q + 4p + 1` splits the phase into a pre-twiddle in `p`,
the FFT kernel `2*pi*pq/(M/2)`, and a post-twiddle in `q`, with the leftover `theta` halved between
the two ends. What falls out is `Z[2q] = Re(S[q])` and `Z[M-1-2q] = -Im(S[q])`; the odd-`j` case is
the even one turned by `i`, because `e^{i(pi/2)(4p+1)}` is `i` for every `p`.

**The direct sum stays in the file and is not dead code.** It is the fast transform's oracle: the
suite runs both on random spectra at every block size the format allows and requires them to agree
to 1e-9. They agree to between 8e-15 and 1.6e-12, growing with the block size the way accumulated
double rounding does. End to end against ffmpeg a transform bug and a residue bug look exactly
alike, which is why the transform has an oracle that owes nothing to either.

## What is left

Nothing pressing. The remaining time is spread across the residue decode, the floor synthesis and
the codebook walk rather than concentrated anywhere.
