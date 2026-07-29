import sys, numpy as np, wave
def readwav(p):
    w=wave.open(p,'rb'); ch=w.getnchannels()
    a=np.frombuffer(w.readframes(w.getnframes()),dtype=np.int16).astype(np.float64).reshape(-1,ch)
    w.close(); return a, ch
def find_shift(x, y, maxlag=1600, w=20000):
    # returns s such that x[n] ~ y[n+s]
    N=min(len(x),len(y)); c=N//2; w=min(w,N//3)
    best=(-2.0,0)
    for s in range(-maxlag, maxlag):
        j=c+s
        if j<0 or j+w>len(y) or c+w>len(x): continue
        cc=np.corrcoef(x[c:c+w], y[j:j+w])[0,1]
        if cc>best[0]: best=(cc,s)
    return best[1]
def metrics(ref, tst):
    a,cha=readwav(ref); b,chb=readwav(tst)
    ch=min(cha,chb); a=a[:,:ch]; b=b[:,:ch]
    s=find_shift(a[:,0], b[:,0])
    if s>=0: B=b[s:]; A=a[:len(B)]
    else:    A=a[-s:]; B=b[:len(A)]
    m=min(len(A),len(B)); A=A[:m]; B=B[:m]
    t=min(4410,m//10); A=A[t:m-t]; B=B[t:m-t]
    corr=np.corrcoef(A.flatten(),B.flatten())[0,1]
    rms=np.sqrt(np.mean((A-B)**2)); rr=np.sqrt(np.mean(A**2))+1e-9
    return corr,rms,rms/rr,len(A),ch,s
if __name__=='__main__':
    corr,rms,nrms,m,ch,s=metrics(sys.argv[1],sys.argv[2])
    print(f"corr={corr:.6f} rms={rms:.2f} nrms={nrms:.4f} n={m} ch={ch} shift={s}")
