# TODO

Version 1: no backend thing

- [X] model the harmonica holes/bends/overblows
- [X] model the chromatic scale
- [ ] model the positions (link hole to a musical interval)
  - [X] 1st,
  - [X] 2nd,
  - [ ] 3rd only
- [ ] make a parser to identify the notation in a text box
    - [X] Parser
    - [ ] UI
- [ ] translate from one position to another
  - [ ] function to create candidates
  - [ ] UI to select the alternatives
    - [ ] option to apply selection globally
- [ ] octave transformer 
  - translate from one position to the same
  - [ ] selector to choose the direction

# How to model the harmonica layout

What we have: 

- 10 holes, draw/blow
- some have 1 or more bends, some have blows
- some have overblows/overdraws

