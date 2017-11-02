# Tutorial 6

If you have implemented `sequ`, `input` and `output` correctly, the
following interaction should work in GHCi.

```
λ> runIO (sequ input output)
Hello
Hello
λ> runIO (sequ input (\x -> sequ input (\y -> output (x ++ y))))
Hello
HElllOOOO
HelloHElllOOOO
```

