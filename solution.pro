% basak tepe
% 2020400117
% compiling: yes
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
%extra predicate
get_first_element([K-V|_], K-V).

%nearest agent(+StateId, +AgentId, -NearestAgentId, -Distance)

evaluate_all_distances(Agents, AgentId, NearestAgentId, ShortestDistance) :-
    %write('here6'),
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
    %write('here7'),
    % Sort the distances in ascending order
    keysort(Distances, SortedDistances),
    write(SortedDistances),
    % Get the nearest agent id and distance
    %SortedDistances = [Distance-NearestAgentId|_],
    length(SortedDistances, Length),
    (Length>0 -> get_first_element(SortedDistances, ShortestDistance-NearestAgentId); NearestAgentId = 0, ShortestDistance = 0).


nearest_agent(0, 0, -999, -999).

nearest_agent(StateId, AgentId, NearestAgentId, Distance) :-
    state(StateId, Agents, _, TurnOrder),
    length(TurnOrder, NumAgents),

    (NumAgents =< 1 -> 
        NearestAgentId = -999,
        Distance = -999
    ;
        NumAgents > 1,
        evaluate_all_distances(Agents, AgentId, NearestAgentId, Distance)
    ).



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



%extra predicates

find_all_portal_traversable_states(0, 0, []).

find_all_portal_traversable_states(StateId, AgentId, PortalTraversableStates) :-
    state(StateId, Agents, CurrentTurn, TurnOrder),
    Agent = Agents.get(AgentId),

    findall(PortalTargetStateId,
    %a state is traversable if it can be reached by portal action.
    (       % can_perform(Agent.class, portal),
            history(StateId, UniverseId, Time, _),
            state(StateId, Agents, CurrentTurn, TurnOrder),
            % Agent = Agents.get(AgentId),
            % check whether global universe limit has been reached
            global_universe_id(GlobalUniverseId),
            universe_limit(UniverseLimit),
            GlobalUniverseId < UniverseLimit,
            % agent cannot time travel if there is only one agent in the universe
            length(TurnOrder, NumAgents),
            NumAgents > 1,
            % [TargetUniverseId, TargetTime] = ActionArgs,
            history(_, TargetUniverseId, TargetTime, _),
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
            \+tile_occupied(Agent.x, Agent.y, TargetState),
            difficulty_of_state(PortalTargetStateId, Agent.name, Agent.class, Difficulty),
            % The difficulty of the easiest traversable state should be greater than zero. 
            Difficulty>0
        ),
        PortalTraversableStates).

        %i should also add the current state and its difficulty before sorting the list.
        %difficulty_of_state(StateId, Agent.name, Agent.class, CurrentStateDifficulty),
        %(CurrentStateDifficulty>0 -> FullPList = [CurrentStateDifficulty-StateId | PortalTraversableStates]; FullPList = PortalTraversableStates),
        %write(PortalTraversableStates).


find_all_portal_to_now_traversable_states(0, 0, []).

find_all_portal_to_now_traversable_states(StateId, AgentId, PortalToNowTraversableStates) :-
    state(StateId, Agents, CurrentTurn, TurnOrder),
    Agent = Agents.get(AgentId),

    findall(PortalToNowTargetStateId,
        %a state is traversable if it can be reached by portal to now action.
        (   can_perform(Agent.class, portal_to_now), 
            % get_current_agent_and_state(UniverseId, AgentId, StateId),
            history(StateId, UniverseId, Time, _),
            state(StateId, Agents, CurrentTurn, TurnOrder),
            Agent = Agents.get(AgentId),
            % agent cannot time travel if there is only one agent in the universe
            length(TurnOrder, NumAgents),
            NumAgents > 1,
            % [TargetUniverseId] = ActionArgs,
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
            \+tile_occupied(Agent.x, Agent.y, TargetState),
            difficulty_of_state(PortalToNowTargetStateId, Agent.name, Agent.class, Difficulty),
            % The difficulty of the easiest traversable state should be greater than zero. 
            Difficulty>0
        ),
        PortalToNowTraversableStates).
        %i should also add the current state and its difficulty before sorting the list.
        %difficulty_of_state(StateId, Agent.name, Agent.class, CurrentStateDifficulty),
        %(CurrentStateDifficulty>0 -> FullPTNList = [CurrentStateDifficulty-StateId | PortalToNowTraversableStates]; FullPTNList = PortalToNowTraversableStates),
        %write(PortalToNowTraversableStates).


find_all_traversable_states(0, 0, []).

find_all_traversable_states(StateId, AgentId, TraversableStates) :-
        state(StateId, Agents, _, _),
        Agent = Agents.get(AgentId),

        find_all_portal_traversable_states(StateId, AgentId, PortalTraversableStates),
        find_all_portal_to_now_traversable_states(StateId, AgentId, PortalToNowTraversableStates),
        append(PortalTraversableStates, PortalToNowTraversableStates, TraversableStatesWithoutCurrent),
        %write(TraversableStatesWithoutCurrent),
        difficulty_of_state(StateId, Agent.name, Agent.class, CurrentStateDifficulty),
        (CurrentStateDifficulty>0 -> TraversableStates = [StateId | TraversableStatesWithoutCurrent]; TraversableStates = TraversableStatesWithoutCurrent).
        %write(TraversableStates).



easiest_traversable_state(0, 0, 0):- !.

easiest_traversable_state(StateId, AgentId, TargetStateId) :- 
        state(StateId, Agents, _, _),
        Agent = Agents.get(AgentId),

    findall(
        Difficulty-PossibleStateId, 
        ((
            find_all_traversable_states(StateId, AgentId, TraversableStates),
            member(PossibleStateId,TraversableStates),
            difficulty_of_state(PossibleStateId, Agent.name, Agent.class, Difficulty),
            % The difficulty of the easiest traversable state should be greater than zero. 
            Difficulty > 0
        )),
        StateDifficulties
        ),


        %write(FullList),
        keysort(StateDifficulties, SortedStateDifficulties),
        SortedStateDifficulties = [Difficulty-TargetStateId|_].


%Agent = Agents.get(_), Agent.x = X, Agent.y = Y
%State = state(StateId, Agents, CurrentTurn, TurnOrder)
%history = history(CandidateStateId, UniverseId, Time, Turn).
%size = length(_, NumAgents).

%get_universe_id(StateId, UniverseId) :- history(StateId, UniverseId, _, _).
%get_time(StateId, Time) :- history(StateId, _, Time, _).
%get_agents(StateId,Agents) :- state(StateId, Agents, _, _).


%PREDICATE 8
%extra predicate


%basic action policy(+StateId, +AgentId, -Action)
basic_action_policy(0, 0, _):-!.

basic_action_policy(StateId, AgentId, Action) :-

    state(StateId, Agents, _, _),
    Agent = Agents.get(AgentId),
    nearest_agent(StateId, AgentId, NearestAgentId, Distance),

    %what does it return next to portal in outputs?
    
    %1. The agent should try to portal to the easiest traversable state if possible  (if allowed)
    (((find_all_portal_traversable_states(StateId, AgentId, Portalables),

    easiest_traversable_state(StateId, AgentId, EasiestTargetStateId),
    length(Portalables, PortalableCount),
    PortalableCount =\= 0,

    %we want the portalable state to either be the easiest one
    member(EasiestTargetStateId,Portalables),

    write(Portalables),

    history(EasiestTargetStateId, UniverseId, _, _),
    
    Action = [portal,UniverseId]);

    (find_all_portal_to_now_traversable_states(StateId, AgentId, PortalToNowables),
    easiest_traversable_state(StateId, AgentId, EasiestTargetStateId),
    length(PortalToNowables, PortalToNowableCount),
    PortalToNowableCount =\= 0,
    
    write(PortalToNowables),

    member(EasiestTargetStateId,PortalToNowables),

    history(EasiestTargetStateId, UniverseId, _, _),
    
    Action = [portal_to_now,UniverseId]));

    %4. If the agent cannot execute the above actions, it should rest.
    (NearestAgentId =:= -999 -> 
    %write('THE AGENT SHOULD REST'),
    Action = [rest]);


    %2. If it cannot travel to the easiest traversable state, it should approach to the nearest different
    %right up left down agent until it is in the attack range of the nearest agent.
    %there has to be a nearest agent to do so.

    (NearestAgentId \= -999 -> NearestAgent = Agents.get(NearestAgentId),

    ((Distance> 1, Agent.class = warrior ->
    write('got to warrior'),
    (Agent.x < NearestAgent.x -> Action = [move_right];
    Agent.y < NearestAgent.y -> Action = [move_up];
    Agent.x > NearestAgent.x -> Action = [move_left];
    Agent.y > NearestAgent.y -> Action = [move_down]
    ));
    (Distance> 15, Agent.class = rogue ->
    write('got to rogue'),
    (Agent.x < NearestAgent.x -> Action = [move_right];
    Agent.y < NearestAgent.y -> Action = [move_up];
    Agent.x > NearestAgent.x -> Action = [move_left];
    Agent.y > NearestAgent.y -> Action = [move_down]
    ));
    (Distance> 10,  Agent.class = wizard ->
    write('got to wizard'),
    (Agent.x < NearestAgent.x -> Action = [move_right];
    Agent.y < NearestAgent.y -> Action = [move_up];
    Agent.x > NearestAgent.x -> Action = [move_left];
    Agent.y > NearestAgent.y -> Action = [move_down]
    ))));


    %3. Once the agent is in the attack range of the nearest agent, it should attack the nearest agent.
    ((Distance=<1, Agent.class = warrior-> Action = [melee_attack, NearestAgentId]);
    (Distance=<15,  Agent.class = rogue -> Action = [ranged_attack, NearestAgentId]);
    (Distance=<10, Agent.class = wizard -> Action = [magic_missile,  NearestAgentId]))).

