### Proof by Weak Induction

> Prove 1 + 2 + 3 + ... + n = [n(n+1)]/2

**Basis: The most basic step** 
n = 1

1 = [1(1+1)/2]; TRUE

**Induction**

Assume True n = k, 1 + 2 + 3 ... + k = [k(k+1)]/2
Show that n = k + 1

1. [1 + 2 + 3... k] + (k+1) = (k+1)(k+1+1)/2<br />
       
2. [k(k+1)]/2] + 2(k+1)/2 = (k+1)(k+1+1)/2<br />
       
3. k(k+1) + 2(k+1) = (k+1)(k+2)<br />
       
4. (k^2) + k + 2k + 2 = (k^2) + 2k + k + 2<br />

5. True

> Prove 1 + 3 + 5 + ... + (2n-1) = (n^2)
