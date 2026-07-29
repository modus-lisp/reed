#!/usr/bin/env python3
"""Delay-align two WAVs (cross-correlation) and report Pearson correlation + RMS.
Usage: aac-compare.py reed.wav ref.wav [label]"""
import sys, wave, numpy as np

def read_wav(path):
    w = wave.open(path, 'rb')
    n, ch, sw = w.getnframes(), w.getnchannels(), w.getsampwidth()
    raw = w.readframes(n); w.close()
    assert sw == 2
    a = np.frombuffer(raw, dtype='<i2').astype(np.float64)
    a = a.reshape(-1, ch)
    return a, w.getframerate() if False else None

def load(path):
    w = wave.open(path,'rb'); ch=w.getnchannels(); n=w.getnframes()
    a=np.frombuffer(w.readframes(n),dtype='<i2').astype(np.float64).reshape(-1,ch); w.close()
    return a

def best_lag(a, b, maxlag=4096):
    # align using channel 0, search integer lag via cross-correlation on a window
    x = a[:,0]; y = b[:,0]
    L = min(len(x), len(y), 200000)
    x = x[:L] - x[:L].mean(); y = y[:L] - y[:L].mean()
    # coarse search
    best, bl = -2, 0
    for lag in range(-maxlag, maxlag+1, 1):
        if lag >= 0:
            xx = x[lag:]; yy = y[:len(xx)]
        else:
            yy = y[-lag:]; xx = x[:len(yy)]
        m = min(len(xx), len(yy))
        if m < 1000: continue
        xx=xx[:m]; yy=yy[:m]
        d = np.sqrt((xx*xx).sum()*(yy*yy).sum())
        if d==0: continue
        c = (xx*yy).sum()/d
        if c > best: best, bl = c, lag
    return bl

def main():
    reed_p, ref_p = sys.argv[1], sys.argv[2]
    label = sys.argv[3] if len(sys.argv)>3 else reed_p
    a = load(reed_p); b = load(ref_p)
    ch = min(a.shape[1], b.shape[1])
    a=a[:,:ch]; b=b[:,:ch]
    lag = best_lag(a,b)
    if lag>=0: a2=a[lag:]; b2=b[:len(a2)]
    else: b2=b[-lag:]; a2=a[:len(b2)]
    m=min(len(a2),len(b2)); a2=a2[:m]; b2=b2[:m]
    # trim first/last 2048 (priming edges)
    if m>6000: a2=a2[2048:-2048]; b2=b2[2048:-2048]
    cors=[]; rmss=[]
    for c in range(ch):
        x=a2[:,c]; y=b2[:,c]
        xd=x-x.mean(); yd=y-y.mean()
        d=np.sqrt((xd*xd).sum()*(yd*yd).sum())
        cor = (xd*yd).sum()/d if d>0 else float('nan')
        rms = np.sqrt(((x-y)**2).mean())
        refrms = np.sqrt((y*y).mean())+1e-9
        cors.append(cor); rmss.append(rms/refrms)
    print("%-22s lag=%-5d ch=%d corr=%s rms_rel=%s" % (
        label, lag, ch,
        ",".join("%.6f"%c for c in cors),
        ",".join("%.4f"%r for r in rmss)))
    return min(cors)

if __name__=="__main__":
    c=main()
    sys.exit(0 if (c is not None and c>0.99) else 1)
