# rl
Scala reinforcement learning framework

This is code I'm using to play around with different reinforcement learning and n-armed bandit algorithms.
I'm also using it to mess around with Dotty/Scala 3. Someone else might find it useful. I plan to work through 
Barto and Sutton's book using this library and repository.

For now I have the Breeze jars committed to the repo because I had to compile them with my version of the JVM 
to get them to work. I'll try to fix that at some point. Feel free to add issues or open PRs. Though I don't 
have a formal process in place right now.

How to Use the Code

You need Dotty. So [follow this process](https://dotty.epfl.ch) to install Dotty. Then you can use this like 
any other Scala app that uses sbt. For example, you can run `sbt console` from the `/book/` directory. In 
console, you can try running something like `bartosutton.exercise.two.figure2dot2(generatePlots = true)`.

In the future I'll include some examples on how to use the framework and various algorithms. Though for now, a 
good example can be found in the function `bartosutton.exercise.two.figure2dot2`. As I add more algorithms, 
I'll move them around to be consumed more like a library.