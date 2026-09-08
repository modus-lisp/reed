# Notes on the FLAC decoder

Sixteen fixtures, each checked twice: against the MD5 the encoder wrote into STREAMINFO, and against
ffmpeg's decode byte for byte. Not a correlation — `equalp`.

## The only codec here that carries its own oracle

Every other decoder in reed is lossy, so its suite asserts a correlation or an RMS bound against
ffmpeg. FLAC has exactly one right answer, and the format records it: STREAMINFO holds the **MD5 of
the unencoded audio**. Decoding and hashing the result compares this decoder against the *encoder's*
record of the original, with no reference decoder anywhere in the loop. If this decoder and ffmpeg
were wrong in the same way — which is not far-fetched, since both were written from the same
specification — the MD5 would still catch it.

The MD5 is over the samples as they would sit in a WAV file: little-endian, interleaved, at the
native bit depth. That last part matters: for the 24-bit fixtures the hash is over three-byte
samples, so it also checks the bit depth handling that the 16-bit path never touches.

**And every frame carries a CRC-16 which the decoder checks as it goes**, plus a CRC-8 over the
header. Those are not decoration. A decoder that desynchronises by one bit anywhere in a subframe
goes on producing plausible-looking samples indefinitely; the CRC is what turns that into a stated
error. It is the same role that "consumes its partition exactly" plays for VP9 in the sibling
repository — a check that is available before there is any output to compare.

## There is no transform

Prediction plus residual, and nothing else. Each subframe predicts its samples from the ones before
— with one of five fixed polynomial predictors, or with up to 32 learned coefficients and a shift —
and codes what the prediction got wrong as a partitioned Rice code. No transform, no
psychoacoustics, no quantisation. That is why the whole decoder is two files and why it runs at
110-170x real time without anything having been optimised.

Three details in that path are easy to get subtly wrong:

**The LPC coefficients run backwards in time.** The first multiplies the sample immediately before
the one being predicted, the second the one before that. Reversing them decodes to noise, but only
for the LPC subframes, so a file that happens to use fixed predictors throughout still sounds fine.

**The prediction shift is arithmetic.** For a negative sum, shifting right is not dividing.

**The side channel carries one extra bit.** A difference of two n-bit numbers needs n+1 bits, and
which of the two subframes is the side one depends on the channel assignment: it is the second for
left/side and mid/side, and the *first* for side/right. Getting that wrong costs one bit of range
on loud passages only, so quiet music decodes correctly and loud music clips.

## The mid/side reconstruction hides a bit in the side channel

The mid channel is stored shifted right by one, which would lose its least-significant bit — except
that an odd mid sample always comes with an odd side sample, so the bit can be recovered from the
side channel: `mid = (mid << 1) | (side & 1)`. That is what makes mid/side stereo lossless rather
than nearly so, and it is one line that is easy to leave out because leaving it out is inaudible.

## Coverage is asserted, and two paths had to be forced

Constant, verbatim, fixed and LPC subframes; wasted bits; both Rice parameter widths; all four
channel assignments. The suite counts them and requires each to be positive.

Two would not appear on their own:

- **ffmpeg's encoder picks left/side almost always**, so side/right and mid/side had to be requested
  by name with `-ch_mode`. The reference encoder's *default* is mid/side, so files in the wild use
  it constantly — it would have been an untested path covering a large fraction of real files.
- **Verbatim subframes need genuinely incompressible input.** Filtered noise is not enough;
  `anoisesrc` is band-limited and a fixed predictor still beats storing it. Uniformly random samples
  straight from `/dev/urandom` are what finally made an encoder give up.

Escaped partitions — a partition storing raw residuals instead of Rice codes — are counted and
reported but not asserted. Nothing in the corpus produces one, and rather than contrive a fixture
the count is printed so it is visible when one ever does.

## Corrupt input

A decoder eats untrusted bytes. Corrupt bits can produce a predictor order, a coefficient and a
residual that are each individually legal and jointly absurd, and the arithmetic downstream then
fails however it fails. The frame CRC would catch it, but only *after* the subframes are decoded —
so anything that goes wrong before that point is caught and reported as a broken frame rather than
escaping as whatever low-level condition it happened to be. The spec's own residual range limit
(§9.2.7.3, everything fits a signed 32-bit integer) is checked where the residual is read, which is
where a corrupt unary run runs away first.
