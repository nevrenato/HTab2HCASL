signature { automatic } theory
{
n1:(n3:<R1>p3 v (n4 --> <R3> p3));
[R1]<R3>(n3:<R3>[R3] (p1 v p2 v p3) <--> n4:n2);
n1:n2 v n2:n1;
[R2](<R1>p1 <--> <R3>p4);
<R2>([R1] (p2 v !p4)) v [R3](p3 --> <R2>(<R2>!p2));
(<R2>(P32 v P31) & <R2>(!P32 v !P31)) <--> p32
}                                                                                             

