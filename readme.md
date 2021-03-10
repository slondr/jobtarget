# JobTarget Programming Assignment

## Technical Overview

For this assignment, I implemented my solution in Erlang. I'm very familiar with Erlang's data and concurrency model, which I previously used in my academic study of concurrent programming.

## Implementation

I spawn three different threads in this program: 2 are used to provide safe and fast concurrent access to global data structures, and a third to implement the main algorithm's event loop.

For the algorithm itself, I use a form of the top-down recursive dynamic programming (DP) paradigm. This algorithm provides greatly increased runtime speed compared to naive algorithms; the space complexity is also impressive, and I noticed no significant memory usage at any point during runtime in my system monitor.

On my system, the program completes and returns a result in about 70 seconds.

## Building and Running

Once you have Erlang installed, running this program is easy. Just clone the repository, navigate into the project's directory, open up the Erlang shell by typing `erl` in your terminal, and then:

1. Run `c(jt).` to compile my program. `jt`, short for JobTarget, is the name of the module; `c/1` is the function which compiles modules. Assuming you ran `erl` in the same directory as the jt.erl file, this will compile and load all my code.
2. Run `jt:start().` to launch the entry point of my program. This program handles the spawning of the 3 run-time threads, which handle the rest!

Note that because the program is implemented asynchronously, you will see the Erlang prompt reappear after running `jt:start().` because start() exits immediately after spawning the 3 threads. The program runs in the background, and when it is complete the result is printed into the shell. If you close Erlang while the program is running in the background, it will kill the process, so don't do that if you want an answer!

## Answer

My program produced the result 732506. Due to the large size of the triangle I can't easily verify by hand if this is correct; however, I tested my program on smaller, more easily hand-parsable triangles and it consistently produced the correct result, so I am highly confident this answer is correct.
