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

difficulty_of_state(0, _, 0, 0) :-!.

difficulty_of_state(StateId, Name, AgentClass, Difficulty) :-

    num_agents_in_state(StateId, Name, NumWarriors, NumWizards, NumRogues),

    (AgentClass == warrior -> 
        Difficulty is 5 * NumWarriors + 8 * NumWizards + 2 * NumRogues
    ;
    AgentClass == wizard ->
        Difficulty is 2 * NumWarriors + 5 * NumWizards + 8 * NumRogues
    ;
    AgentClass == rogue ->
        Difficulty is 8 * NumWarriors + 2 * NumWizards + 5 * NumRogues
    ).


%PREDICATE 7
% easiest_traversable_state(+StateId, +AgentId, -TargetStateId).

%This predicate will return the easiest traversable state for a given agent. A state is traversable if
%the agent can reach it by using portal or portal to now actions. The given state is assumed to be
%traversable as well. If there are multiple states with the same difficulty, you are free to return any
%of them. The difficulty of the easiest traversable state should be greater than zero. You will use
%difficulty of state for sorting states by difficulty.

%extra predicate 
%find_all_universe_state_time_combinations(0,0,0,[]).

%find_all_universe_state_time_combinations(UniverseId,Time,StateId,UniverseStateTimeCombinations):-
 %   findall(
  %      UniverseId - StateId - Time,
   %     (
    %        history(StateId, UniverseId, Time, _),
     %   ),
      %  UniverseStateTimeCombinations).



%extra predicate
find_all_traversable_states(0, 0, []).
find_all_traversable_states(StateId, AgentId, TraversableStates) :-

    state(StateId, Agents, CurrentTurn, TurnOrder),
    Agent = Agents.get(AgentId),

    findall(PortalTargetStateId,
    %a state is traversable if it can be reached by portal action.
    (       can_perform(Agent.class, portal),
            write('here'),
            get_current_agent_and_state(UniverseId, AgentId, StateId),
            write('exit'),
            history(StateId, UniverseId, Time, Turn),
            State = state(StateId, Agents, CurrentTurn, TurnOrder),
            Agent = Agents.get(AgentId),
            % check whether global universe limit has been reached
            global_universe_id(GlobalUniverseId),
            universe_limit(UniverseLimit),
            GlobalUniverseId < UniverseLimit,
            % agent cannot time travel if there is only one agent in the universe
            length(TurnOrder, NumAgents),
            NumAgents > 1,
            [TargetUniverseId, TargetTime] = ActionArgs,
            % check whether target is now or in the past
            current_time(TargetUniverseId, TargetUniCurrentTime, _),
            TargetTime < TargetUniCurrentTime,
            % check whether there is enough mana
            (Agent.class = wizard -> TravelCost = 2; TravelCost = 5),
            Cost is abs(TargetTime - Time)*TravelCost + abs(TargetUniverseId - UniverseId)*TravelCost,
            Agent.mana >= Cost,
            % check whether the target location is occupied
            get_earliest_target_state(TargetUniverseId, TargetTime, PortalTargetStateId),
            state(PortalTargetStateId, TargetAgents, _, TargetTurnOrder),
            TargetState = state(PortalTargetStateId, TargetAgents, _, TargetTurnOrder),
            \+tile_occupied(Agent.x, Agent.y, TargetState)
        ),
        PortalTraversableStates),

        findall(PortalToNowTargetStateId,
        %a state is traversable if it can be reached by portal to now action.
        (   can_perform(Agent.class, portal_to_now), 
            get_current_agent_and_state(UniverseId, AgentId, StateId),
            history(StateId, UniverseId, Time, Turn),
            State = state(StateId, Agents, CurrentTurn, TurnOrder),
            Agent = Agents.get(AgentId),
            % agent cannot time travel if there is only one agent in the universe
            length(TurnOrder, NumAgents),
            NumAgents > 1,
            [TargetUniverseId] = ActionArgs,
            % agent can only travel to now if it's the first turn in the target universe
            current_time(TargetUniverseId, TargetTime, 0),
            % agent cannot travel to current universe's now (would be a no-op)
            \+(TargetUniverseId = UniverseId),
            % check whether there is enough mana
            (Agent.class = wizard -> TravelCost = 2; TravelCost = 5),
            Cost is abs(TargetTime - Time)*TravelCost + abs(TargetUniverseId - UniverseId)*TravelCost,
            Agent.mana >= Cost,
            % check whether the target location is occupied
            get_latest_target_state(TargetUniverseId, TargetTime, PortalToNowTargetStateId),
            state(PortalToNowTargetStateId, TargetAgents, _, TargetTurnOrder),
            TargetState = state(PortalToNowTargetStateId, TargetAgents, _, TargetTurnOrder),
            \+tile_occupied(Agent.x, Agent.y, TargetState)
        ),
        PortalToNowTraversableStates),

        append(PortalTraversableStates, PortalToNowTraversableStates, TraversableStates),
        write(TraversableStates).




easiest_traversable_state(0, 0, 0):- !.

easiest_traversable_state(StateId, AgentId, TargetStateId) :- 
        state(StateId, Agents, _, _),
        Agent = Agents.get(AgentId),

    findall(
        Difficulty-PossibleStateId, 
        ((
            find_all_traversable_states(StateId, AgentId, TraversableStates),
            %bozuk
            write(TraversableStates),
            member(PossibleStateId,TraversableStates),
            difficulty_of_state(PossibleStateId, Agent.name, Agent.class, Difficulty)
        )),
        StateDifficulties
        ),

        %i should also add the current state and its difficulty before sorting the list.
        difficulty_of_state(StateId, Agent.name, Agent.class, CurrentStateDifficulty),
        %write(StateDifficulties),
        %write(CurrentStateDifficulty-StateId),
        %adding the current state and its difficulty to the list
        FullList = [CurrentStateDifficulty-StateId | StateDifficulties],
        %write(FullList),
        keysort(FullList, SortedStateDifficulties),
        write(SortedStateDifficulties),
        SortedStateDifficulties = [Difficulty-TargetStateId|_].

    % The difficulty of the easiest traversable state should be greater than zero. 
    %if difficulty is smaller than 0, obtain the next difficulty in the list 







%Agent = Agents.get(_), Agent.x = X, Agent.y = Y
%State = state(StateId, Agents, CurrentTurn, TurnOrder)
%history = history(CandidateStateId, UniverseId, Time, Turn).
%size = length(_, NumAgents).

%get_universe_id(StateId, UniverseId) :- history(StateId, UniverseId, _, _).
%get_time(StateId, Time) :- history(StateId, _, Time, _).
%get_agents(StateId,Agents) :- state(StateId, Agents, _, _).




% basic_action_policy(StateId, AgentId, Action).
