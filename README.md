sml-streams
===========

Streams in Standard ML of New Jersey.

### Test

Install MLton (e.g. for fedora):
```shell
sudo yum install mlton
```
Compile with MLton:
```shell
mlton streams.sml
```
and run:
```shell
./streams
```
### Benchmarks
```shell

(Outer=5000000 , Inner=50)
Streams cart time: 1.857 sec/op
Baseline cart time: 1.195 sec/op
```

### References

* [Clash of the Lambdas](http://arxiv.org/abs/1406.6631)
* [Nessos/Streams](https://github.com/nessos/Streams)
* [MLton](http://mlton.org/)
