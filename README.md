# mazesolver

This is a demo project showcasing the usage of:

- [Cats-collections' DisjointSets](https://typelevel.org/cats-collections//disjointsets.html)
- [Akka-HTTP WebSockets](https://doc.akka.io/docs/akka-http/current/server-side/websocket-support.html)
- [Akka-Streams](https://doc.akka.io/docs/akka/current/stream/index.html)
- [Scala.js](https://www.scala-js.org/)

Working together to build a 2D map exploration algorithm laboratory on which:

![image](https://user-images.githubusercontent.com/273379/105969059-1f302600-6088-11eb-8dd4-95c03973d100.png)

- A server side is responsible for:
  - Generating sample imputs.
  - Receiving resolution requests.
  - Fullfilling the resolution request, providing asynchronous events on the progress that can be represented at UI side as well as a final event with the completed solution.
  
- A client side is responsible for:
  - Allowing loading files with problems to resolve.
  - Configuring the resolution algorithm.
  - Receiving resolution update and finalization events and representing them.

## Problem

Given a 2D map where we have walls or open cells, and to which you can only enter through the open cells at the matrix borders (doors). Provide the set of sets of doors that are connected to each other.
In other words, that answers to the question: Which doors can I reach from each other door?

From:

![seas](https://user-images.githubusercontent.com/273379/105971081-63bcc100-608a-11eb-9c86-19dae3cf2b94.png)

To:

![seas_resolved](https://user-images.githubusercontent.com/273379/105971148-7e8f3580-608a-11eb-9f62-6b4d35735988.png)

## Code structure

This is a multi-module project with the server side being compiled for the JVM, the client side as JS and a common set of entities compiled for both JVM and JS:

- [jvm](./jvm/src/main/scala/org/pfcoperez/mazesolver): Server side application:
  - [Solver.scala](jvm/src/main/scala/org/pfcoperez/mazesolver/Solver.scala) Resolution algorithm implementation as two functions (`initialConditions`)[https://github.com/pfcoperez/mazesolver/blob/main/jvm/src/main/scala/org/pfcoperez/mazesolver/Solver.scala#L95], providing the first resolution step given the input, and `explorationStep`, generating the next resolution step out of a previous one. This represents a recursive search algorithm decomposed so it can be used to generate a stream of solutions.
  - [Generator.scala](jvm/src/main/scala/org/pfcoperez/mazesolver/Generator.scala) An input generator.
  - [Server.scala](https://github.com/pfcoperez/mazesolver/blob/main/jvm/src/main/scala/org/pfcoperez/mazesolver/Server.scala) Solutions stream and its utilization to provide a WS interface.
  - [DisjointSetsWrapper.scala](https://github.com/pfcoperez/mazesolver/blob/main/jvm/src/main/scala/org/pfcoperez/mazesolver/datastructures/DisjointSetsWrapper.scala) An interface abstracting different implementations of DisjointSets. It provides factories for the _cats-collections_ implementation and for a non-too-functional-nor-efficient approach.
- [shared](https://github.com/pfcoperez/mazesolver/tree/main/shared/src/main/scala/org/pfcoperez/mazesolver) Entities comprising the communication protocol between the UI and the server.
- [js](./js/src/main/scala/org/pfcoperez/mazesolver) UI Implementation using Scala.JS. This code is not as curated as the server side, it doesn't follow the  functional programming paradigm. Take it as a support tool for the real demo, the server side.
