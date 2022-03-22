:- dynamic([
  world_size/1, % size of the model as [X, Y]
  position/2,   % (A, [X, Y]) <=> position of A is [X, Y] 
  wumpus/1,   % position of Wumpus -> inferred from smell
  noWumpus/1,   % no Wumpus in [X, Y] -> inferrence: no smell on adjacent cells
  noPit/1,    % no Wumpus in [X, Y] -> inferrence: no smell on adjacent cells
  maybeVisitLater/2,  % add current to visit later and backtrack
  goldPath/1,   % path from [1, 1] cell to gold
  score/1   %score of the game
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% BEGIN EXECUTION %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run :-
  % clear previous stored facts
  retractall(position(_, _)),
  retractall(wumpus(_)),
  retractall(pit(_)),
  retractall(noWumpus(_)),
  retractall(maybeVisitLater(_,_)),
  retractall(goldPath(_)),
  retractall(score(_)),

  assert(score(0)),

  % initialize model
  init,

  % agent begins search from [1, 1] cell
  search([1, 1], []),

  % if any paths stored as possible to visit later, do so
  maybeVisitLater(PausedCell, LeadingPath),
  retract(maybeVisitLater(PausedCell, _)),
  search(PausedCell, LeadingPath).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% MODEL 1 %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init :-
  retractall(world_size(_)),
  assert(world_size([4, 4])),   % dimensions of the world

  retractall(position(_, _)),
  assert(position(gold, [2, 3])),   % position of gold

  % pits positions
  assert(position(pit, [3, 1])),
  assert(position(pit, [3, 3])),
  assert(position(pit, [4, 4])),

  assert(noPit([1, 1])),    % no pit at [1, 1] where agent starts

  % INITIALIZING AGENT POSITION
  assert(position(agent, [1, 1])),

  % INITIALIZING WUMPUS POSITION
  assert(position(wumpus, [1, 3])),
  assert(noWumpus([1, 1])). % no wumpus at [1, 1] where agent starts

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% MODEL 2 %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init :-
  retractall(world_size(_)),
  assert(world_size([4, 4])),   % dimensions of the world

  retractall(position(_, _)),
  assert(position(gold, [1, 2])),   % position of gold

  % pits positions
  assert(position(pit, [3, 1])),
  assert(position(pit, [3, 3])),
  assert(position(pit, [1, 4])),
  assert(position(pit, [4, 4])),

  assert(noPit([1, 1])),    % no pit at [1, 1] where agent starts

  % INITIALIZING AGENT POSITION
  assert(position(agent, [1, 1])),

  % INITIALIZING WUMPUS POSITION
  assert(position(wumpus, [2, 4])),
  assert(noWumpus([1, 1])). % no wumpus at [1, 1] where agent starts
*/
/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%% MODEL 3 %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init :-
  retractall(world_size(_)),
  assert(world_size([3, 3])),   % dimensions of the world

  retractall(position(_, _)),
  assert(position(gold, [3, 1])),   % position of gold

  % pits positions
  assert(position(pit, [1, 2])),
  assert(position(pit, [2, 3])),
  assert(position(pit, [3, 3])),

  assert(noPit([1, 1])),    % no pit at [1, 1] where agent starts

  % INITIALIZING AGENT POSITION
  assert(position(agent, [1, 1])),

  % INITIALIZING WUMPUS POSITION
  assert(position(wumpus, [2, 2])),
  assert(noWumpus([1, 1])). % no wumpus at [1, 1] where agent starts
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% SENSORS %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% predicate to check if cell position is valid (replaces bumping into a wall)
valid_position([X, Y]):- X>0, Y>0, world_size([P, Q]), X@=<P, Y@=<Q.

% adjacent cell Z 
adjacent([X, Y], Z) :- Left is X-1, valid_position([Left, Y]), Z=[Left, Y].
adjacent([X, Y], Z) :- Right is X+1, valid_position([Right, Y]), Z=[Right, Y].
adjacent([X, Y], Z) :- Above is Y+1, valid_position([X, Above]), Z=[X, Above].
adjacent([X, Y], Z) :- Below is Y-1, valid_position([X, Below]), Z=[X, Below].

% smelly room if Wumpus is adjacent to it
smelly([X, Y]):-
  position(wumpus, Z), \+ noWumpus(Z),
  adjacent([X, Y], Z).

% breezy room if pit is adjacent to it
breezy([X, Y]):- adjacent([X, Y], Z), position(pit, Z).

% breezy room if it encompasses gold
glittery([X, Y]):- position(gold, Z), Z==[X, Y].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% ACTIONS %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% predicate to check if two different matches can be found to ascertain Wumpus' location
manyWumpuses:- wumpus(X), wumpus(Y), X\=Y.

% killing Wumpus from a cell facing it
killWumpus(AgentCell):-
  wumpus([Xw, Yw]), \+ manyWumpuses,   
  AgentCell=[Xa, Ya],
  (Xw==Xa; Yw==Ya),     % check if agent is facing the wumpus
  assert(noWumpus([Xw, Yw])),   % wumpus killed
  format('~n+ Wumpus in cell ~w was shot with an arrow from cell ~w and killed!~n', [[Xw, Yw], AgentCell]),
  retractall(wumpus(_)).

% check for wumpus -> if(cell.isSmelly()) then wumpus in adjacent cells => kill it
search(Cell, LeadingPath):-
  smelly(Cell),
  adjacent(Cell, X),
  \+ noWumpus(X), assert(wumpus(X)),
  killWumpus(Cell), %kill the wumpus
  append(LeadingPath, [Cell], CurrentPath),
  score(S),
  Z is S - 1,
  retractall(score(_)),
  assert(score(Z)),
  format('+ Path to killing the wumpus is ~w ~n+ Game score is ~w', [CurrentPath, Z]), !.

% checking if cell contains gold
search(Cell, LeadingPath):-
  glittery(Cell),
  score(S),
  Z is S + 1000 - 1,
  retractall(score(_)),
  assert(score(Z)),
  append(LeadingPath, [Cell], CurrentPath),
  % record the gold path if it's not already done
  \+ goldPath(CurrentPath), assert(goldPath(CurrentPath)).

% check if the agent can perceive breeze in the cell
search(Cell,_):-
  breezy(Cell).
  %format('BREEZY cell!~n').

% check for pits -> if(cell.isNotBreeey()) then no pits in adjacent cells
search(Cell,_):-
  \+ breezy(Cell),
  score(S),
  Z is S - 1,
  retractall(score(_)),
  assert(score(Z)),
  adjacent(Cell, X),
  \+ noPit(X), assert(noPit(X)).

% if cell is not smelly => no wumpus in adjacent cells
search(Cell,_):-
  \+ smelly(Cell),
  adjacent(Cell, X),
  \+ noWumpus(X), assert(noWumpus(X)),
  wumpus(Y), X==Y, retract(wumpus(Y)).

% continue exploring safe cells
search(CurrentCell, LeadingPath):-

  (killWumpus(CurrentCell); format('')),  % Kill Wumpus if possible

  append(LeadingPath, [CurrentCell], CurrentPath),

  score(S),
  Z is S - 1,
  retractall(score(_)),
  assert(score(Z)), 

  % get adjacent cells 
  adjacent(CurrentCell, X), \+ member(X, LeadingPath),

  % if unsafe cells (<=> potentially containing wumpus or pit): visitLater
  (( noWumpus(X), noPit(X)) -> write('');
    (\+ maybeVisitLater(CurrentCell, _) -> assert(maybeVisitLater(CurrentCell, LeadingPath)); write(''))
  ),

  % search starting from no Wumpus/no pit cells
  noWumpus(X), noPit(X),
  search(X, CurrentPath).








