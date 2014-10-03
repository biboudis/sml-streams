sml-streams
===========

Bulk memory operations with Standard ML in MLton. The pattern is based on pipelines that are composed via continuations, where a source is wrapped as a stream, transformations are applied via lazy operations and eager combinators force the effects on the backing, in-memory collection of items:
```
source - lazy - lazy - lazy - eager
```

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
# Outer=1000000, Inner=10
Streams cart time: 0.098 sec/op
Baseline cart time: 0.048 sec/op

# Outer=5000000, Inner=50
Streams cart time: 1.857 sec/op
Baseline cart time: 1.195 sec/op

# Outer=10000000, Inner=100
Streams cart time: 7.151 sec/op
Baseline cart time: 4.723 sec/op
```

### References

* [Clash of the Lambdas](http://arxiv.org/abs/1406.6631)
* [Nessos/Streams](https://github.com/nessos/Streams)
* [MLton](http://mlton.org/)
* Found out that similar pattern appears in [MLton/Iterator combinators](http://mlton.org/ForLoops), [iter.sml](https://github.com/MLton/mltonlib/blob/master/com/ssh/extended-basis/unstable/detail/control/iter.sml) / [iter.sig](https://github.com/MLton/mltonlib/blob/master/com/ssh/extended-basis/unstable/public/control/iter.sig)
