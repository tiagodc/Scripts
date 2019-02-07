## `-thin_with_voxel` TLS benchmark
- CPU: Intel(R) Core(TM) i7-8550U CPU @ 1.80GHz
- cores: 8 (4 physical x 4 threads)
- RAM: 16 GB
- average CPU usage during file streaming: 25%
- reading function: `lidR::readLAS` 

### Cloud 1
- N: 2,541,917 points
- extents: 30 x 30 x 14 m

| Voxel spacing (m) | surviving points | read time (s) | ~ memory usage (GB) |
| --- | --- | --- | --- |
| 0.1 | 349,787 | 1 | 0.03 |
| 0.05 | 925,881 | 1 | 0.12 |
| 0.025 | 1,718,986 | 2 | 0.14 |
| 0.01 | 2,414,604 | 2 | 0.23 |

### Cloud 2
- N: 10,773,659 points
- extents: 86 x 127 x 19 m

| Voxel spacing (m) | surviving points | read time (s) | ~ memory usage (GB) |
| --- | --- | --- | --- |
| 0.1 | 1,049,797 | 6 | 0.28 |
| 0.05 | 2,907,514 | 7 | 0.38 |
| 0.025 | 6,252,097 | 10 | 0.50 |
| 0.01 | 9,947,220 | 13 | 0.81 |

### Cloud 3
- N: 42,869,489 points
- extents: 400 x 217 x 56 m

| Voxel spacing (m) | surviving points | read time (s) | ~ memory usage (GB) |
| --- | --- | --- | --- |
| 0.1 | 2,606,613 | 32 | 0.22 |
| 0.05 | 7,602,382 | 37 | 0.64 |
| 0.025 | 19,556,071 | 50 | 1.68 |
| 0.01 | 37,853,465 | 65 | 3.31 |

### Cloud 4
- N: 165,155,435 points
- extents: 259 x 234 x 193 m

| Voxel spacing (m) | surviving points | read time (s) | ~ memory usage (GB) |
| --- | --- | --- | --- |
| 0.1 | 1,506,216 | 72 | 0.37 |
| 0.05 | 4,552,934 | 77 | 0.82 |
| 0.025 | 12,987,644 | 104 | 0.88 |
| 0.01 | 41,461,462 | 140 | 3.44 |

