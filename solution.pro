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
% multiverse_distance(StateId, AgentId, TargetStateId, TargetAgentId, Distance).

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


evaluate_all_distances(Agents, CompareAgentId, AgentId, NearestAgentId, Distance) :-
    dict_size(Agents,NumAgents), %returns the number of keys in dictionary.
    CompareAgentId < NumAgents, % stop recursion when CompareAgentId exceeds number of agents

    % it has to be a different agent
    (CompareAgentId == AgentId -> NextAgentId is CompareAgentId + 1 ; NextAgentId is CompareAgentId),

    % loop through the agents to find the nearest one
    get_dict(AgentId, Agents, ReferenceAgent),
    get_dict(NextAgentId, Agents, NextAgent),


    distance(ReferenceAgent, NextAgent, CurrentDistance),
    (
        CurrentDistance < Distance ->
            % update the nearest agent and distance if a closer one is found
            NewDistance is CurrentDistance,
            NewNearestAgentId is NextAgentId
        ;   NewDistance is Distance,
            NewNearestAgentId is NearestAgentId
    ),
    % recursively check the next agent 
    NextAgentId is NextAgentId + 1,
    evaluate_all_distances(Agents, NextAgentId, AgentId, NewNearestAgentId, NewDistance).



nearest_agent(0,0,0,0).

nearest_agent(StateId, AgentId, NearestAgentId, Distance) :-
    get_agents(StateId, Agents),
    Distance is 1000000, % a very large number
    evaluate_all_distances(Agents, 0, AgentId, NearestAgentId, Distance).
    




% nearest_agent_in_multiverse(StateId, AgentId, TargetStateId, TargetAgentId, Distance).
% num_agents_in_state(StateId, Name, NumWarriors, NumWizards, NumRogues).
% difficulty_of_state(StateId, Name, AgentClass, Difficulty).
% easiest_traversable_state(StateId, AgentId, TargetStateId).
% basic_action_policy(StateId, AgentId, Action).
