# Markov Models
Why not use Search?
**When you have a non-deterministic situation, you could get stuck**

##Example : Grid World
- Maze problem.
  - agent lives in a grid
  - walls block the agent's path
- Noisy movvement: actions do not go as plan
  - 80% of the time it goes in a direction
  - 20% of the time pushed in a random direction

#Markov Decision Processes
MDP is defined by
- A set of states s in S
- a set of actions a in A
- A transition function T(s, a, s')
  - probability that a form s leads to s',  P(s' | s,a)
  - Called **model** or **the dynamics**
- A reward function R(s, a, s')
  - Sometimes just R(s) or R(s')
- A start state
- Maybe a terminal state

# Policies
In deterministic single-agent search problems, we want an optimal plan, or
sequence of actions from start to a goal

**policy:** __a maping of state to actions__

- We want an optimal policy pi*:S->A
  - A policy gives an action for all states
  - An optimal policy is one that maximizes expected utility if followed
  - An explicit policy defines a reflex agent

- Expectimax didn't compute entire policies
  - It computed the action for a single state only

## Racing Example
states: cool, hot, destroyed
Driving slow has a cool engine
Driving fast has a hot engine
A overheated engine is destroyed

A cool car driving fast becomes hot
A hot car dring slow may or maynot be cool
A hot car driving fast will be destroyed.

###Solution
Each MDP state projects an expectimax-like search tree

##Discounting
To prevent loops and delays, decrease reward. by gama^n
The exponential decay makes it work

##Stationalry Preferences
Theorm: if we assume stationary preferences

Your scheme a is prefered over b
If you add step r to a and r to b, and you still prefer you sequence to b then a is stationary.

**Additive utility**: U([r0, r1, r12]) = r0 + r1 + r2

##Infinite Utilities?
What happens if the game last forever? Do we get infinte rewards?

Solutions
- Finite horizon: (dept limited search)
- Discounting : use 0 < gamma < 1

##Optimal Quantities
- The value (utility) of a state s:
  V*(s) = expected utility starting in s and acting optimally
- The value (utility) of a q-state (s, a):
  Q*(s, a) = expected utility staritng out having tken action a form state s and (thereafter ) acting optimally
- The optimal policy:
  Pi*(s) optimal action from state s
