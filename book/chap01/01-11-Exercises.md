<!--
   01-11-Exercises.md

   1.11 Chapter Exercises, page 17
-->

# Chapter Exercises #

## Combinators ##

Determine if each of the following are combinators or not.

Lambda term      | Is combinator?
-----------------|---------------
1. λx.xxx        | yes
2. λxy.zx        | no (z is a free variable)
3. λxyz.xy(zx)   | yes
4. λxyz.xy(zxy)  | yes
5. λxy.xy(zxy)   | no (z is a free variable)

## Normal for or diverge? ##

Determine if each of the following can be reduced to normal form
or if they diverge.

Lambda term       | Is in normal form?   | Diverges?
------------------|----------------------|----------
1. λx.xxx         | yes                  | no
2. (λz.zz)(λy.yy) | no                   | yes (omega term)
3. (λx.xxx)z      | yes (reduces to zzz) | no

## Beta reduce ##

Evaluate (that is beta reduce) each oif the following expressions
to mnormal form.

1. (λabc.cba)zz(λwv.w)  
   (λa.λb.λc.cba)zz(λw.λv.w)
   (λb.λc.cbz)z(λw.λv.w)  
   (λc.czz)(λw.λv.w)  
   (λw.λv.w)zz  
   (λv.z)z  
   z  

2. (λx.λy.xyy)(λa.a)b  
   (λy.(λa.a)yy)b  
   (λa.a)bb  
   bb  

3. (λy.y)(λx.xx)(λz.zq)  
   (λx.xx)(λz.zq)  
   (λz.zq)(λz.zq)  
   (λz.zq)q  
   qq  

4. (λz.z)(λz.zz)(λz.zy) // Hint: alpha equivalence  
   (λz.zz)(λz.zy)  
   (λz.zy)(λz.zy)  
   (λz.zy)y  
   yy  

5. (λx.λy.xyy)(λy.y)y
   (λy.(λy.y)yy)y  
   (λy.y)yy  
   yy  

6. (λa.aa)(λb.ba)c  
   (λb.ba)ac  
   aac  

7. (λxyz.xz(yz))(λx.z)(λx.a)  
   (λx.λy.λz.xz(yz))(λx.z)(λx.a)  
   (λy.λz.(λx.z')z(yz))(λx.a)  
   (λz.(λx.z')z((λx.a)z))  
   λz.(λx.z')z((λx.a)z)  
   λz.z'((λx.a)z)  
   λz.z'a
