% basak tepe
% 2020400117
% compiling: no
% complete: no



distance(0, 0, 0).  % a dummy predicate to make the sim work.

% PREDICATE 1
%distance(Agent, TargetAgent, Distance).

distance(Agent, TargetAgent, Distance):-
    Distance is abs( Agent.x - TargetAgent.x) + abs(Agent.y - TargetAgent.y).



%another method with intermediate values
%DiffX is abs(X1 - X2),
%DiffY is abs(Y1 - Y2),
%Distance is DiffX + DiffY


%Agent = Agents.get(_), Agent.x = X, Agent.y = Y
%State = state(StateId, Agents, CurrentTurn, TurnOrder)
%history = history(CandidateStateId, UniverseId, Time, Turn).
%size = length(_, NumAgents).



% PREDICATE 2
% multiverse_distance(+StateId, +AgentId, +TargetStateId, +TargetAgentId, -Distance).

%we needed extra predicates.
get_universe_id(StateId, UniverseId) :- history(StateId, UniverseId, _, _).
get_time(StateId, Time) :- history(StateId, _, Time, _).
get_agents(StateId,Agents) :- state(StateId, Agents, _, _).

multiverse_distance(0,0,0,0,0). %dummy


multiverse_distance(StateId, AgentId, TargetStateId, TargetAgentId, Distance) :-
    %TravelCost is (Class = wizard -> 2 ; 5),   
    get_agents(StateId,Agents), 
    get_agents(TargetStateId,TargetAgents), 
    (Agents.get(AgentId).class = wizard -> TravelCost = 2; TravelCost = 5),

    get_time(StateId, Time),
    get_time(TargetStateId, TargetTime),
    get_universe_id(StateId, UniverseId),
    get_universe_id(TargetStateId, TargetUniverseId),

    %travel cost is variant.

    Distance is abs(Agents.get(AgentId).x - TargetAgents.get(TargetAgentId).x) +  
                abs(Agents.get(AgentId).y - TargetAgents.get(TargetAgentId).y) +
              TravelCost * (abs(Time - TargetTime) + abs(UniverseId - TargetUniverseId)).


%PREDICATE 3
% nearest_agent(StateId, AgentId, NearestAgentId, Distance).
%nearest agent(+StateId, +AgentId, -NearestAgentId, -Distance)

evaluate_all_distances(Agents, AgentId, NearestAgentId, Distance) :-
    %names should also be different
    findall(
        CurrentDistance-NextAgentId,
        (
            % Get the reference and next agents
            ReferenceAgent = Agents.get(AgentId),
            NextAgent = Agents.get(NextAgentId),

            % Calculate the distance between the reference and next agents
            distance(ReferenceAgent, NextAgent, CurrentDistance),

            % Make sure the next agent is not the reference agent and the names are also different
            NextAgentId \= AgentId,
            ReferenceAgent.name \=  NextAgent.name
        ),
        Distances
    ),
    % Sort the distances in ascending order
    keysort(Distances, SortedDistances),
    % Get the nearest agent id and distance
    SortedDistances = [NewDistance-NewNearestAgentId|_],
    Distance is NewDistance,
    NearestAgentId is NewNearestAgentId.


nearest_agent(0, 0, 0, 0).

nearest_agent(StateId, AgentId, NearestAgentId, Distance) :-
    get_agents(StateId, Agents),
    evaluate_all_distances(Agents, AgentId, NearestAgentId, Distance).



%PREDICATE 4
% nearest_agent_in_multiverse(StateId, AgentId, TargetStateId, TargetAgentId, Distance).
%similar to 3rd predicate but we have to do 2 findalls.
%1 to traverse all universes and 1 to traverse all states (agent lists)


evaluate_all_multiuniversal_distances(StateId, AgentId, NearestStateId, NearestAgentId, Distance) :-
    get_agents(StateId, Agents),

    findall(
        CurrentDistance-TargetAgentId-TargetStateId,
        (
            % Get the reference and next agents
            ReferenceAgent = Agents.get(AgentId),
            TargetAgent = Agents.get(TargetAgentId),

            % Calculate the distance between the reference and next agents
            % multiverse_distance(+StateId, +AgentId, +TargetStateId, +TargetAgentId, -Distance).
            multiverse_distance(StateId, AgentId, TargetStateId,TargetAgentId, CurrentDistance),

            % Make sure the next agent is not the reference agent and the names are also different
            TargetAgentId \= AgentId,
            ReferenceAgent.name \=  TargetAgent.name
        ),
        Distances
    ),
    % Sort the distances in ascending order
    keysort(Distances, SortedDistances),
    % Get the nearest agent id and distance
    SortedDistances = [NewDistance-NewNearestAgentId-NewNearestStateId|_],
    Distance is NewDistance,
    NearestAgentId is NewNearestAgentId,
    NearestStateId is NewNearestStateId.



nearest_agent_in_multiverse(0, 0, 0, 0, 0).

%nearest agent in multiverse(+StateId, +AgentId, -TargetStateId, -TargetAgentId, -Distance)
nearest_agent_in_multiverse(StateId, AgentId, NearestStateId, NearestAgentId, Distance) :-
    evaluate_all_multiuniversal_distances(StateId, AgentId, NearestStateId, NearestAgentId, Distance).


%PREDICATE 5
% num_agents_in_state(+StateId, +Name, -NumWarriors, -NumWizards, -NumRogues).

num_agents_in_state(0, _, 0, 0, 0).

num_agents_in_state(StateId, Name, NumWarriors, NumWizards, NumRogues) :-
    get_agents(StateId,Agents),

    findall(
        Agent, 
        (
            Agent = Agents.get(AgentId),
            Agent.name \= Name,    
            Agent.class == rogue
        ),
        Rogues
    ),

    findall(
        Agent, 
        (
            Agent = Agents.get(AgentId),
            Agent.name \= Name,    
            Agent.class == wizard
        ),
        Wizards
    ),

    findall(
        Agent, 
        (
            Agent = Agents.get(AgentId),
            Agent.name \= Name,    
            Agent.class == warrior
        ),
        Warriors
    ),

    length(Rogues,NumRogues),
    length(Warriors,NumWarriors),
    length(Wizards,NumWizards).






%PREDICATE 6
% difficulty_of_state(+StateId, +Name, +AgentClass, -Difficulty).

difficulty_of_state(0, _, 0, 0).


%Agent = Agents.get(_), Agent.x = X, Agent.y = Y
%State = state(StateId, Agents, CurrentTurn, TurnOrder)
%history = history(CandidateStateId, UniverseId, Time, Turn).
%size = length(_, NumAgents).

%get_universe_id(StateId, UniverseId) :- history(StateId, UniverseId, _, _).
%get_time(StateId, Time) :- history(StateId, _, Time, _).
%get_agents(StateId,Agents) :- state(StateId, Agents, _, _).










% easiest_traversable_state(StateId, AgentId, TargetStateId).
% basic_action_policy(StateId, AgentId, Action).
