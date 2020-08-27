###

1. Prove base case, n = 1
2. n = k 
3. Prove n = k + 1 by replacing stuff

### Proof by Weak Induction

> Prove 1 + 2 + 3 + ... + n = [n(n+1)]/2

**Basis: The most basic step n = 1** 

1 = [1(1+1)/2]; TRUE

**Induction**

- Assume True n = k, 1 + 2 + 3 ... + k = [k(k+1)]/2
- Show that n = k + 1

1. **[1 + 2 + 3... k] + (k+1) = (k+1)(k+1+1)/2**<br />
       
2. **[k(k+1)]/2] + 2(k+1)/2 = (k+1)(k+1+1)/2**<br />
       
3. **k(k+1) + 2(k+1) = (k+1)(k+2)**<br />
       
4. **(k^2) + k + 2k + 2 = (k^2) + 2k + k + 2**<br />

5. **TRUE**

> Prove 1 + 3 + 5 + ... + (2n-1) = (n^2)

**Basis: The most basic step n = 1**

1 = (1^2); TRUE

**Induction**

- Assume True n = k, 1 + 3 + 5 + ... + (2k-1)  = (k^2)
- Show that n = k + 1 

1. **[1+3+5+...+(2k-1)] + (2k+2-1) = [(k+1)^2]**

2. **(k^2) + (2k + 1) = [(k+1)^2]**

3. **(k^2) + 2k + 1 = (k^2) + 2k + 1

4. **TRUE**

>  <img src="https://i.imgur.com/XaJ1c0e.png"/>

**Basis: The most basic step n = 1**
1 = [1(1+1)(2(1)+1)]/6; TRUE

**induction** 

- Assume True n = k, 1 + 4 + 9 + ... + (k^2) = [k(k+1)(2k+1)]/6
- Show that n = k + 1 

1. **1 + 4 + 9 + ... + (k^2) + ((k+1)^2) = [(k+1)(k+1+1)(2(k+1)+1)]/6 **

2. **[k(k+1)(2k+1)]/6 + ((k+1)^2) = [(k+1)(k+1+1)(2(k+1)+1)]/6**

3. **[k(k+1)(2k+1)] + 6((k+1)^2) = [(k+1)(k+1+1)(2(k+1)+1)]** 

4. 2(k^3) + 3(k^2) + k + 6(k^2) + 12k + 6 = (n+1)(n+2)(2n+3)

5. 2k^3 + 9k^2 + 12k + 6
