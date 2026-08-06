#!/usr/bin/env python3
"""Build the two MP4 encoder-delay fixtures for inspect/mp4-delay-gate.lisp.

An AAC encoder cannot start at sample zero, and the container is where the
resulting priming is declared.  There are two ways to declare it and a decoder
has to read both, so there are two fixtures:

  delay_elst.m4a       the standard mechanism — edts/elst media_time
  delay_itunsmpb.m4a   Apple's — the iTunSMPB text tag in udta/meta/ilst

Both are the same audio, and both declare the same 1024-sample delay, so both
must decode to the same samples as delay_ref.wav with no offset at all.

The elst file is what ffmpeg's own muxer writes (media_time = 1024); nothing is
edited into it.  The iTunSMPB file is made from it by *zeroing* the elst and
adding the tag, so that the only thing left that can produce the right answer is
the tag.  A fixture that declared the delay twice would pass while reading
either one.

    python3 test/gen-mp4-delay.py            # from the repo root
"""
import struct, subprocess, sys, os

SRC = "aac-corpus/music_s_128.aac"
OUT_ELST = "aac-corpus/delay_elst.m4a"
OUT_TAG = "aac-corpus/delay_itunsmpb.m4a"
OUT_REF = "aac-corpus/delay_ref.wav"
SECONDS = "3"


def boxes(b, start, end):
    p = start
    while p + 8 <= end:
        sz = struct.unpack(">I", b[p:p + 4])[0]
        nm = b[p + 4:p + 8].decode("latin1")
        if sz == 0:
            sz = end - p
        yield nm, p, sz
        p += sz


def find(b, start, end, name):
    for nm, p, sz in boxes(b, start, end):
        if nm == name:
            return p, sz
    return None, None


def descend(b, path):
    p, sz = 0, len(b)
    s, e = 0, len(b)
    for name in path:
        p, sz = find(b, s, e, name)
        if p is None:
            raise SystemExit(f"no {name} box")
        # meta is a FullBox: 4 bytes of version/flags before its children
        s = p + 8 + (4 if name == "meta" else 0)
        e = p + sz
    return p, sz


def main():
    if not os.path.exists(SRC):
        raise SystemExit(f"run from the repo root; {SRC} not found")
    subprocess.run(["ffmpeg", "-v", "error", "-y", "-i", SRC, "-t", SECONDS,
                    "-c:a", "aac", "-b:a", "128k", OUT_ELST], check=True)
    subprocess.run(["ffmpeg", "-v", "error", "-y", "-i", OUT_ELST,
                    "-c:a", "pcm_s16le", OUT_REF], check=True)

    b = bytearray(open(OUT_ELST, "rb").read())

    # what the muxer declared, and how many samples the table actually holds
    ep, _ = descend(b, ["moov", "trak", "edts", "elst"])
    ver = b[ep + 8]
    if ver != 0:
        raise SystemExit("version-1 elst; this generator only handles version 0")
    delay = struct.unpack(">i", b[ep + 20:ep + 24])[0]
    zp, zsz = descend(b, ["moov", "trak", "mdia", "minf", "stbl", "stsz"])
    total = struct.unpack(">I", b[zp + 16:zp + 20])[0] * 1024
    if delay <= 0:
        raise SystemExit(f"ffmpeg declared no delay (media_time {delay}); "
                         "nothing to gate")

    # top-level moov must come last, or inserting into it would shift mdat and
    # invalidate every absolute chunk offset in stco
    tops = [nm for nm, _, _ in boxes(b, 0, len(b))]
    if tops[-1] != "moov":
        raise SystemExit(f"moov is not last ({tops}); would have to fix stco")

    tag = bytearray(b)
    # zero the elst, so only the tag can say where the audio starts
    tag[ep + 20:ep + 24] = struct.pack(">i", 0)

    # iTunSMPB: fixed-width ASCII, delay / padding / original sample count
    original = total - delay
    text = (" 00000000 %08X %08X %016X" % (delay, 0, original)
            + " 00000000" * 8)
    data = b"data" + struct.pack(">II", 1, 0) + text.encode("ascii")
    data = struct.pack(">I", len(data) + 4) + data
    mean = b"mean" + struct.pack(">I", 0) + b"com.apple.iTunes"
    mean = struct.pack(">I", len(mean) + 4) + mean
    name = b"name" + struct.pack(">I", 0) + b"iTunSMPB"
    name = struct.pack(">I", len(name) + 4) + name
    free = b"----" + mean + name + data
    free = struct.pack(">I", len(free) + 4) + free
    ilst = struct.pack(">I", len(free) + 8) + b"ilst" + free
    hdlr = (b"hdlr" + bytes(8) + b"mdir" + b"appl" + bytes(9))
    hdlr = struct.pack(">I", len(hdlr) + 4) + hdlr
    meta = b"meta" + bytes(4) + hdlr + ilst
    meta = struct.pack(">I", len(meta) + 4) + meta
    udta = struct.pack(">I", len(meta) + 8) + b"udta" + meta

    mp, msz = find(tag, 0, len(tag), "moov")
    tag[mp:mp + 4] = struct.pack(">I", msz + len(udta))
    tag[mp + msz:mp + msz] = udta          # udta goes at the end of moov
    open(OUT_TAG, "wb").write(bytes(tag))

    print(f"{OUT_ELST}: elst media_time {delay}, {total} samples decoded")
    print(f"{OUT_TAG}: elst zeroed, iTunSMPB delay {delay} padding 0 "
          f"original {original}")
    print(f"{OUT_REF}: ffmpeg's own decode, delay already removed")


main()
