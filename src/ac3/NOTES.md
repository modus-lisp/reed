# Notes on the AC-3 decoder

Six fixtures against ffmpeg at correlations from 0.99986 to 1.000000 — mono, stereo at three bit
rates, 5.1 with the LFE channel, and 32 kHz. AC-3 is why a DVD or a transport stream used to open
here, show its picture, and name the audio as undecodable.

## The bit allocation is a psychoacoustic model both ends run

Nothing in an AC-3 bitstream says how many bits a coefficient got. The decoder works it out: from
the exponents alone it computes a power spectral density, integrates it into fifty critical bands,
spreads it with a leaky excitation function, clamps it against a hearing threshold, and reads a bit
allocation pointer off a table. The encoder did exactly that to decide what to spend; the decoder
does it to find out what was spent.

That makes the failure mode absolute. Get one step wrong and every mantissa from that point on is
read at the wrong width, so the parse drifts and the rest of the frame is noise. There is no
graceful degradation anywhere in this codec — which is also what makes it easy to debug, because
"the frame consumed 3306 of its 3328 bits" is a complete statement about correctness. That check is
the AC-3 equivalent of VP9's "consumes its partition exactly" in the sibling repository.

## Four bugs, and what each one looked like

Worth recording because each had a distinct signature, and the signature is what found it.

**`ff_ac3_floor_tab` is `int16_t` and its last entry is written `0xf800`.** Read unsigned that is
63488 rather than −2048, the masking floor goes to the moon, every bit allocation pointer comes out
zero, and the decoder reads no mantissas at all — a frame that parses to 672 bits out of 3328. The
table extractor now takes signedness from the declared C type, and the invariant that the floor
decreases would have caught it too.

**The chincpl bits.** ffmpeg makes the coupling flags implicit for stereo, but only for E-AC-3; the
guard is `s->eac3 && channel_mode == STEREO` and a grep that filtered out lines containing "eac3"
turned it into an unconditional shortcut. Two bits per block, and the parse drifted from the
exponent strategies onward.

**The delta bit allocation enum.** `REUSE=0, NEW=1, NONE=2, RESERVED=3` — reading NEW as 2 meant the
segment fields were never consumed, so everything up to the first block that used them was perfect
and everything after was noise.

**The coupling coordinates were eight times too small**, and the tail of every coupled channel was
being wiped after coupling had filled it. Both are visible only in stereo at low bit rates, where
coupling covers most of the spectrum — the second one showed up as one channel missing 5 to 8 kHz
entirely while the other was correct to six figures, which is about as clear as a diagnostic gets.

The transform scale was the fifth, and it announced itself the way Vorbis's did in this repository:
correlation near one against the reference with a relative error near one, which is a pure gain
error and nothing else. Here it was 1024 exactly.

## Dither is why this cannot reach one

A coefficient the allocator gave no bits is filled with noise (§7.3.4), so that an uncoded band
sounds like a noise floor rather than a hole. **The sequence is explicitly pseudo-random and not
normative** — no two AC-3 decoders produce the same samples there.

That sets the bar, and the evidence is clean: decoding with dither suppressed and comparing again
gives a residual of exactly 1/√2 of the one with dither, which is what two independent noise
sequences of equal power do and nothing else does. At 384 kbit/s almost nothing is dithered and the
match is 1.000000 with a relative error of one part in ten thousand; at 96 kbit/s a great deal is
and the error is one part in two hundred.

## What is not here

**E-AC-3** (bitstream id 16) is a different format wearing the same sync word, and is refused by
name rather than half-decoded. So is enhanced coupling.

**No downmix.** A 5.1 stream decodes to six channels, in the order a player expects rather than the
order AC-3 codes them, and what to do with them is the caller's decision — reed's mixer will fold
them if asked. Baking one listening decision into a decoder is the kind of thing that cannot be
undone by a caller who wanted the other one.

**Three paths are implemented and untested**, and it is the encoder's fault rather than the
decoder's: ffmpeg's AC-3 encoder does no transient detection, so it never switches to short blocks;
it never emits a delta bit allocation; and it never sets the coupling phase flags. The suite counts
all three and prints them rather than asserting on them. A Dolby-encoded DVD track would cover the
first two, and finding one is what it would take to turn those lines into assertions.
