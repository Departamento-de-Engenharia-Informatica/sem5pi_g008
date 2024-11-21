﻿
:- dynamic availability/3.%:- retractall(availability(_,_,_)).
:-dynamic agenda_operation_room/3.
:-dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_operation_room1/3.
:-dynamic assignment_surgery/2.


%another example
agenda_staff(d001,20241028,[]).
agenda_staff(d002,20241028,[]).
agenda_staff(d003,20241028,[]).
agenda_staff(d004,20241028,[]).
agenda_staff(d005,20241028,[]).
agenda_staff(d006,20241028,[]).
agenda_staff(d007,20241028,[]).
agenda_staff(d008,20241028,[]).

timetable(d001,20241028,(480,1200)).
timetable(d002,20241028,(720,1440)).
timetable(d003,20241028,(600,1320)).
timetable(d004,20241028,(520,1320)).
timetable(d005,20241028,(520,1440)).
timetable(d006,20241028,(520,1440)).
timetable(d007,20241028,(520,1440)).
timetable(d008,20241028,(520,1440)).

%another example
%timetable(d001,20241028,(480,1200)).
%timetable(d002,20241028,(500,1440)).
%timetable(d003,20241028,(520,1320)).
%timetable(d004,20241028,(520,1320)).
%timetable(d005,20241028,(520,1320)).
%timetable(d006,20241028,(520,1320)).



staff(d001,doctor,orthopaedist,[so2,so3,so4]).
staff(d002,doctor,orthopaedist,[so2,so3,so4]).
staff(d003, doctor, anesthetist, [so2, so3]).
staff(d004, doctor, orthopaedist, [so2]).
staff(d005, doctor, cleaner, [so2]).
staff(d006, doctor, cleaner, [so2]).
staff(d007, doctor, anesthetist, [so2, so3]).
staff(d008, doctor, anesthetist, [so2, so3]).

surgery_duration(so3, 10). % Exemplo: cirurgia so3 dura 120 minutos.
surgery_duration(so2, 90).  % Exemplo: cirurgia so2 dura 90 minutos.
surgery_duration(so4, 90).  % Exemplo: cirurgia so2 dura 90 minutos.


% Requisitos de staff para cirurgias

surgery_staff_requirements(operation_team,so2, [(orthopaedist, 1)]).
surgery_staff_requirements(anesthetist_team,so2, [(anesthetist, 1)]).
surgery_staff_requirements(cleaning_team,so2, [(cleaner, 1)]).

surgery_staff_requirements(operation_team,so3, [(orthopaedist, 1)]).
surgery_staff_requirements(anesthetist_team,so3, [(anesthetist, 1)]).
surgery_staff_requirements(cleaning_team,so3, [(cleaner, 1)]).

surgery_staff_requirements(operation_team,so4, [(orthopaedist, 1)]).
surgery_staff_requirements(anesthetist_team,so4, [(anesthetist, 1)]).
surgery_staff_requirements(cleaning_team,so4, [(cleaner, 1)]).

agenda_operation_room(or1,20241028,[(520,579,so100001)]).
agenda_operation_room(ola,20241028,[]).


surgery(so2,45,60,45).
surgery(so3,45,90,45).
surgery(so4,45,75,45).

surgery_id(so100001,so2).
surgery_id(so100002,so3).
surgery_id(so100003,so4).
surgery_id(so100004,so2).
surgery_id(so100005,so4).

assignment_surgery(so100001,d001).






find_free_agendas(Date):-retractall(availability(_,_,_)),findall(_,(agenda_staff(D,Date,L),free_agenda_staff0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),_).


free_agenda_staff0([],[(0,1440)]).
free_agenda_staff0([(0,Tfin,_)|LT],LT1):-!,free_agenda_staff1([(0,Tfin,_)|LT],LT1).
free_agenda_staff0([(Tin,Tfin,_)|LT],[(0,T1)|LT1]):- T1 is Tin-1,
    free_agenda_staff1([(Tin,Tfin,_)|LT],LT1).

free_agenda_staff1([(_,Tfin,_)],[(T1,1440)]):-Tfin\==1440,!,T1 is Tfin+1.
free_agenda_staff1([(_,_,_)],[]).
free_agenda_staff1([(_,T,_),(T1,Tfin2,_)|LT],LT1):-Tx is T+1,T1==Tx,!,
    free_agenda_staff1([(T1,Tfin2,_)|LT],LT1).
free_agenda_staff1([(_,Tfin1,_),(Tin2,Tfin2,_)|LT],[(T1,T2)|LT1]):-T1 is Tfin1+1,T2 is Tin2-1,
    free_agenda_staff1([(Tin2,Tfin2,_)|LT],LT1).


adapt_timetable(D,Date,LFA,LFA2):-timetable(D,Date,(InTime,FinTime)),treatin(InTime,LFA,LFA1),treatfin(FinTime,LFA1,LFA2).

treatin(InTime,[(In,Fin)|LFA],[(In,Fin)|LFA]):-InTime=<In,!.
treatin(InTime,[(_,Fin)|LFA],LFA1):-InTime>Fin,!,treatin(InTime,LFA,LFA1).
treatin(InTime,[(_,Fin)|LFA],[(InTime,Fin)|LFA]).
treatin(_,[],[]).

treatfin(FinTime,[(In,Fin)|LFA],[(In,Fin)|LFA1]):-FinTime>=Fin,!,treatfin(FinTime,LFA,LFA1).
treatfin(FinTime,[(In,_)|_],[]):-FinTime=<In,!.
treatfin(FinTime,[(In,_)|_],[(In,FinTime)]).
treatfin(_,[],[]).


intersect_all_agendas([Name],Date,LA):-!,availability(Name,Date,LA).
intersect_all_agendas([Name|LNames],Date,LI):-
    availability(Name,Date,LA),
    intersect_all_agendas(LNames,Date,LI1),
    intersect_2_agendas(LA,LI1,LI).

intersect_2_agendas([],_,[]).
intersect_2_agendas([D|LD],LA,LIT):-	intersect_availability(D,LA,LI,LA1),
					intersect_2_agendas(LD,LA1,LID),
					append(LI,LID,LIT).

intersect_availability((_,_),[],[],[]).

intersect_availability((_,Fim),[(Ini1,Fim1)|LD],[],[(Ini1,Fim1)|LD]):-
		Fim<Ini1,!.

intersect_availability((Ini,Fim),[(_,Fim1)|LD],LI,LA):-
		Ini>Fim1,!,
		intersect_availability((Ini,Fim),LD,LI,LA).

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)],[(Fim,Fim1)|LD]):-
		Fim1>Fim,!,
		min_max(Ini,Ini1,_,Imax),
		min_max(Fim,Fim1,Fmin,_).

intersect_availability((Ini,Fim),[(Ini1,Fim1)|LD],[(Imax,Fmin)|LI],LA):-
		Fim>=Fim1,!,
		min_max(Ini,Ini1,_,Imax),
		min_max(Fim,Fim1,Fmin,_),
		intersect_availability((Fim1,Fim),LD,LI,LA).


min_max(I,I1,I,I1):- I<I1,!.
min_max(I,I1,I1,I).


timed_search_pending_surgeries(Date, Room) :-
    % Registrar o tempo inicial
    statistics(runtime, [Start|_]),
    
    % Executar o método principal
    search_pending_surgeries(Date, Room),
    
    % Registrar o tempo final
    statistics(runtime, [End|_]),
    
    % Calcular o tempo total
    Runtime is End - Start,
    
    % Exibir o resultado
    format('Tempo de execução: ~w ms\n', [Runtime]),
        format('Tempo de execução end: ~w ms\n', [End]),
    format('Tempo de execução start: ~w ms\n', [Start]).



    % Predicado para listar e agendar cirurgias pendentes
search_pending_surgeries(Date, Room) :-
    statistics(runtime, [Start|_]),

    findall(Surgery, surgery_id(Surgery, _), SurgeryList),
    schedule_pending_surgeries(SurgeryList,Date,Room),
        statistics(runtime, [End|_]),

                format('Tempo de execução end: ~w ms\n', [End]),
            format('Tempo de execução start: ~w ms\n', [Start]).

     
% Caso base: quando a lista de cirurgias está vazia.
schedule_pending_surgeries([], _, _) :-
    format('Todas as cirurgias pendentes foram processadas.\n', []).

% Caso recursivo: processa a cirurgia atual e continua com o restante.
schedule_pending_surgeries([X | Rest], Date, Room) :-
    (   
        schedule_surgery(X, Date, Room) ->
        format('Cirurgia ~w agendada com sucesso na sala ~w na data ~w.\n\n\n', [X, Room, Date])
    ;  
        format('Falha ao agendar a cirurgia ~w na sala ~w na data ~w.\n\n', [X, Room, Date])
    ),
    schedule_pending_surgeries(Rest, Date, Room).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
schedule_surgery(Surgery, Date, Room) :-
    % Obter o tipo de cirurgia
    surgery_id(Surgery, Tipo),
    format('Iniciando agendamento para a cirurgia ~w...\n', [Surgery]),
  
    surgery(Tipo,Time_Anesthesia,Time_Surgery,Time_Cleaning),
        Duration is Time_Anesthesia + Time_Surgery + Time_Cleaning,
        format('Detalhes da cirurgia:~n'),
        format(' - Tipo: ~w~n', [Tipo]),
        format(' - Tempo de Anestesia: ~d minutos~n', [Time_Anesthesia]),
        format(' - Tempo de Cirurgia: ~d minutos~n', [Time_Surgery]),
        format(' - Tempo de Limpeza: ~d minutos~n', [Time_Cleaning]),
        format(' - Tempo total de ocupação da sala: ~d minutos~n', [Duration]),

    % Obter os requisitos de staff para a cirurgia
    surgery_staff_requirements(operation_team,Tipo, Requirements),
    surgery_staff_requirements(anesthetist_team,Tipo,Requirements2),
    surgery_staff_requirements(cleaning_team,Tipo, Requirements3),
    format('Requisitos de staff para a cirugia ~w: ~w\n', [Surgery, Requirements]),
    format('Requisitos de staff para a anestesia ~w: ~w\n', [Surgery, Requirements2]),
    format('Requisitos de staff para a limpeza ~w: ~w\n', [Surgery, Requirements3]),
        
    
    % Obter a duração da cirurgia
    % TODO: retirar esta parte e subtiyuir duration por TotalTime
    format('Duração estimada para a cirurgia ~w: ~w minutos\n', [Surgery, Duration]),

    % Encontrar agendas livres
    find_free_agendas(Date),

    % Obter a lista de staff para a cirurgia
    get_staff_for_surgery(Requirements, StaffList),
        format('Equipes disponíveis para a cirurgia ~w: ~w\n', [Surgery, StaffList]),
    
    get_staff_for_surgery(Requirements2, Staff_AnesthesiaList),
        format('Equipes disponíveis para a anestesia ~w: ~w\n', [Surgery, Staff_AnesthesiaList]),
    
    get_staff_for_surgery(Requirements3, Staff_CleaningList),
        format('Equipes disponíveis para a limpeza ~w: ~w\n', [Surgery, Staff_CleaningList]),

    

    % Encontrar intervalos comuns entre as equipes
    findall(CommonIntervals, (member(Team, StaffList), intersect_all_agendas(Team, Date, CommonIntervals)), AllCommonIntervals),
       % format('Intervalos comuns encontrados para equipa de operação: ~w\n', [AllCommonIntervals]),
    
    findall(CommonIntervals, (member(Team, StaffList), intersect_all_agendas(Team, Date, CommonIntervals)), AllCommonIntervals_Anesthesia),
        %format('Intervalos comuns encontrados para  equipa de anestesia: ~w\n', [AllCommonIntervals_Anesthesia]),
        
    findall(CommonIntervals, (member(Team, StaffList), intersect_all_agendas(Team, Date, CommonIntervals)), AllCommonIntervals_Cleaning),
        %format('Intervalos comuns encontrados para a equipa de limpeza: ~w\n', [AllCommonIntervals_Cleaning]),

    % Verificar se há intervalos suficientes para realizar a cirurgia
    findall(SurgeryInterval, (member(CommonIntervals, AllCommonIntervals), select_sufficient_interval(CommonIntervals, Time_Surgery, SurgeryInterval)), AllSurgeryInterval),
        flatten(AllSurgeryInterval, Result_surgery),
              list_to_set(Result_surgery, UniqueResultsurgery),
         format('Intervalos suficientes encontrados para  equipa de operação: ~w\n', [UniqueResultsurgery]),
   
    findall(SurgeryInterval, (member(CommonIntervals, AllCommonIntervals_Anesthesia), select_sufficient_interval(CommonIntervals, Time_Anesthesia, SurgeryInterval)), AllSurgeryInterval_Anesthesia),
        flatten(AllSurgeryInterval_Anesthesia, Result_Anesthesia),
              list_to_set(Result_Anesthesia, UniqueResultAnesthesia),
         format('Intervalos suficientes encontrados para  equipa de anestesia: ~w\n', [UniqueResultAnesthesia]),
   
    findall(SurgeryInterval, (member(CommonIntervals, AllCommonIntervals_Cleaning), select_sufficient_interval(CommonIntervals, Time_Cleaning, SurgeryInterval)), AllSurgeryInterval_Cleaning),
       flatten(AllSurgeryInterval_Cleaning, Result_Cleaning),
             list_to_set(Result_Cleaning, UniqueResultCleaning),
        format('Intervalos suficientes encontrados para a equipa de limpeza: ~w\n', [UniqueResultCleaning]),

    
    precede(UniqueResultAnesthesia,UniqueResultsurgery,UniqueResultCleaning,Result_allCombinatios),
         format('---------------Result_allCombinatios-----------: ~w\n', [Result_allCombinatios]),

%alterar set_new interval
    agenda_operation_room(Room, Date, RoomAgenda),
    findall(NewInterval, (
    member(SurgeryInterval1, Result_allCombinatios),
     set_new_interval(SurgeryInterval1,RoomAgenda,Duration,NewInterval)), StaffRoomIntervals),
              list_to_set(StaffRoomIntervals, StaffRoomIntervals1),
              flatten(StaffRoomIntervals1, StaffRoomIntervals2),
        format('-------------StaffRoomIntervals-----------: ~w\n', [StaffRoomIntervals2]),


    findall(SurgeryInterval, (member(SurgeryInterval, StaffRoomIntervals2), check_room_availability(Room, Date, SurgeryInterval)), ValidRoomIntervals),
    format('Intervalos válidos com a sala disponível: ~w\n', [ValidRoomIntervals]),

 min_final_minute(ValidRoomIntervals, MinInterval, UpdatedList),
    format('Menor hora final selecionado: ~w\n', [MinInterval]),
    format('Salas válidas restantes: ~w\n', [UpdatedList]),

assign_surgery2(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval,Staff_AnesthesiaList,ValidRoomIntervals,Staff_CleaningList,StaffList).


%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       assign_surgery2(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval,Staff_AnesthesiaList,ValidRoomIntervals,Staff_CleaningList,StaffList):-
(      assign_surgery(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval,Staff_AnesthesiaList,Staff_CleaningList,StaffList)
    -> true
    ;
        format('Falha na atribuição. Tentando novamente...\n'),
        min_final_minute(ValidRoomIntervals, MinInterval, UpdatedList),
        format('Menor hora final selecionado: ~w\n', [MinInterval]),
        format('Salas válidas restantes: ~w\n', [UpdatedList]),
        assign_surgery2(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval,Staff_AnesthesiaList,UpdatedList,Staff_CleaningList,StaffList)   
).

assign_surgery(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval,Staff_AnesthesiaList,Staff_CleaningList,StaffList) :-    

    % Encontrar as equipes com o intervalo final mais cedo para a cirurgia
    find_staff_with_min_interval(StaffList, Date, MinInterval, StaffWithMinInterval, Tipo),
    format('Equipes com o intervalo ~w disponível para cirurgia: ~w\n', [MinInterval, StaffWithMinInterval]),

    % Encontrar as equipes de anestesia disponíveis
    find_staff_with_min_intervalAnesthesia(Staff_AnesthesiaList, Date, MinInterval, StaffWithMinInterval_AnesthesiaList, Tipo),
    format('Equipes com o intervalo ~w disponível para anestesia: ~w\n', [MinInterval, StaffWithMinInterval_AnesthesiaList]),

    % Encontrar as equipes de limpeza disponíveis
    find_staff_with_min_intervalClenaing(Staff_CleaningList, Date, MinInterval, StaffWithMinInterval_CleaningLis, Tipo),
    format('Equipes com o intervalo ~w disponível para limpeza: ~w\n', [MinInterval, StaffWithMinInterval_CleaningLis]),

    % Se houver equipes disponíveis, selecionar uma e atualizar as agendas
    [SelectedTeam | _] = StaffWithMinInterval,  % Seleciona a primeira equipe disponível
    format('Equipe selecionada para cirurgia: ~w\n', [SelectedTeam]),
    
    [SelectedTeamAnesthesia | _] = StaffWithMinInterval_AnesthesiaList,  % Seleciona a primeira equipe de anestesia disponível
    format('Equipe selecionada para anestesia: ~w\n', [SelectedTeamAnesthesia]),
    
    [SelectedTeamCleaning | _] = StaffWithMinInterval_CleaningLis,  % Seleciona a primeira equipe de limpeza disponível
    format('Equipe selecionada para limpeza: ~w\n', [SelectedTeamCleaning]),

    % Adicionar a atribuição da cirurgia
    add_assignment_surgery(Surgery, Room),
    format('Cirurgia ~w atribuída à sala ~w.\n', [Surgery, Room]),

    % Ajustar os horários para cirurgia, anestesia e limpeza
    (Start, End) = MinInterval,
    AdjustedStartSurgery is Start + Time_Anesthesia,  
    AdjustedEndSurgery is End - Time_Cleaning,        
    AdjustedStartAnesthesia is Start,  
    AdjustedEndAnesthesia is End - Time_Cleaning,        
    AdjustedStartCleaning is Start + Time_Anesthesia + Time_Surgery,  
    AdjustedEndCleaning is End,        

    % Atualizar a agenda das equipes
    update_staff_agendas(SelectedTeam, Date, (AdjustedStartSurgery, AdjustedEndSurgery), Surgery),   
    format('Agenda da equipe ~w atualizada para a cirurgia ~w.\n', [SelectedTeam, Surgery]),
    
    update_staff_agendas(SelectedTeamAnesthesia, Date, (AdjustedStartAnesthesia, AdjustedEndAnesthesia), Surgery),   
    format('Agenda da equipe ~w atualizada para a cirurgia ~w.\n', [SelectedTeamAnesthesia, Surgery]),
    
    update_staff_agendas(SelectedTeamCleaning, Date, (AdjustedStartCleaning, AdjustedEndCleaning), Surgery),   
    format('Agenda da equipe ~w atualizada para a cirurgia ~w.\n', [SelectedTeamCleaning, Surgery]),

    % Atualizar a agenda da sala
    update_room_agenda(Room, Date, MinInterval, Surgery),
    format('Agenda da sala ~w atualizada para a cirurgia ~w.\n', [Room, Surgery]).
    
assign_surgery(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo) :-
    format('Falha na atribuição. Tentando novamente...\n'),
    assign_surgery(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo).










get_staff_for_surgery([], [[]]).
get_staff_for_surgery([(Type, Quantity) | RestRequirements], AllStaffLists) :-
    findall(ID, staff(ID, _, Type, _), AllStaff), 
    findall(Selected, (combination(Quantity, AllStaff, Selected)), PossibleSelections),
    get_staff_for_surgery(RestRequirements, RestStaffLists), % Recursivamente para o restante dos requisitos.
    findall(AppendedList,
            (member(Selected, PossibleSelections), % Para cada seleção possível do requisito atual...
             member(RestStaff, RestStaffLists),   % ...combine com as possibilidades do resto.
             append(Selected, RestStaff, AppendedList)), 
            AllStaffLists). % Produz todas as combinações possíveis.
           %TODO AllStaffLists\=[].

combination(0, _, []). 
combination(N, [H|T], [H|R]) :- 
    N > 0, 
    N1 is N - 1, 
    combination(N1, T, R).
combination(N, [_|T], R) :- 
    N > 0, 
    combination(N, T, R).


select_sufficient_interval([(Start, End) | Rest], Duration, (Start, End)) :-
    End - Start + 1 >= Duration,
    (Rest = [] ; 
     \+ (member((Start2, End2), Rest), 
         End2 - Start2 + 1 >= Duration, 
         Start2 < Start))
    .
select_sufficient_interval([_ | Rest], Duration, Interval) :-
    select_sufficient_interval(Rest, Duration, Interval).

check_room_availability(Room, Date, (Start, End)) :-
    agenda_operation_room(Room, Date, RoomAgenda),
findall((AStart, AEnd, _), (
        member((AStart, AEnd, _), RoomAgenda),
        Start < AEnd,
        End > AStart
    ), Conflicts),
    Conflicts = []. 



update_staff_agendas([], _, _, _).
update_staff_agendas([ID | Rest], Date, (Start, End), Surgery) :-
    agenda_staff(ID, Date, CurrentAgenda),

    append(CurrentAgenda, [(Start, End, Surgery)], UpdatedAgenda),

    retractall(agenda_staff(ID, Date, _)),  
    assertz(agenda_staff(ID, Date, UpdatedAgenda)),  

    update_staff_agendas(Rest, Date, (Start, End), Surgery).


%Melhorar
%Surgery- tem de ser o id da cirrugia, ou seja tenho de cria uma nova cirurgia
    update_room_agenda(Room, Date, (Start, End), Surgery) :-
    agenda_operation_room(Room, Date, CurrentAgenda),

    append(CurrentAgenda, [(Start, End, Surgery)], UpdatedAgenda),

    retractall(agenda_operation_room(Room, Date, _)),
    assertz(agenda_operation_room(Room, Date, UpdatedAgenda)).

add_assignment_surgery(SurgeryID, Room) :-
    \+assignment_surgery(SurgeryID,_),
    assertz(assignment_surgery(SurgeryID, Room)).
   

% Encontra o intervalo com o menor minuto final em AllCommonIntervals
min_final_minute(AllCommonIntervals, MinInterval,NewList_Unique) :-
%TODO REtirar flanten
    flatten(AllCommonIntervals, Flattened),
    findall((Start, End), member((Start, End), Flattened), Intervals), 
    findall(End, member((_, End), Intervals), EndMinutes), 
    min_list(EndMinutes, MinMinute), % menor minuto final
    member(MinInterval, Intervals), 
    MinInterval = (_, MinMinute), % Encontra o intervalo com o minuto final igual a MinMinute
    list_to_set(AllCommonIntervals, NewList),
    select(MinInterval, NewList, NewList_Unique).
    
   

% Encontra a equipe que tem o intervalo comum igual ao min_interval
find_staff_with_min_interval(StaffList, Date, MinInterval, StaffWithMinInterval,Tipo) :-
    findall(Team, (
        member(Team, StaffList),
        intersect_all_agendas(Team, Date, CommonIntervals),
        is_available_in_intervals(MinInterval, CommonIntervals,Tipo)  % Verifica se MinInterval está na lista de CommonIntervals da equipe
    ), StaffWithMinInterval).
    
 find_staff_with_min_intervalClenaing(StaffList, Date, MinInterval, StaffWithMinInterval,Tipo) :-
     findall(Team, (
         member(Team, StaffList),
         intersect_all_agendas(Team, Date, CommonIntervals),
         is_available_in_intervalsClenaing(MinInterval, CommonIntervals,Tipo)  % Verifica se MinInterval está na lista de CommonIntervals da equipe
     ), StaffWithMinInterval).
     
find_staff_with_min_intervalAnesthesia(StaffList, Date, MinInterval, StaffWithMinInterval,Tipo) :-
    findall(Team, (
        member(Team, StaffList),
        intersect_all_agendas(Team, Date, CommonIntervals),
        is_available_in_intervalsAnesthesia(MinInterval, CommonIntervals,Tipo)  % Verifica se MinInterval está na lista de CommonIntervals da equipe
    ), StaffWithMinInterval).

% Verifica se MinInterval está disponível em CommonIntervals
is_available_in_intervals((Start, End), CommonIntervals,Tipo) :-
    surgery(Tipo,Time_Anesthesia,_,Time_Cleaning),
    member((CStart, CEnd), CommonIntervals),  % Itera sobre CommonIntervals
    CStart =< Start + Time_Anesthesia,  % O início do intervalo comum deve ser menor ou igual ao início de MinInterval
    CEnd >= End - Time_Cleaning.      % O fim do intervalo comum deve ser maior ou igual ao fim de MinInterval

is_available_in_intervalsClenaing((Start, End), CommonIntervals,Tipo) :-
    surgery(Tipo,Time_Anesthesia,Time_Surgery,_),
    member((CStart, CEnd), CommonIntervals),  
    CStart =< Start +Time_Surgery+Time_Anesthesia,  
    CEnd >= End.     

is_available_in_intervalsAnesthesia((Start, End), CommonIntervals,Tipo) :-
    surgery(Tipo,Time_Anesthesia,Time_Surgery,_),
    member((CStart, CEnd), CommonIntervals),
    CStart =< Start+Time_Anesthesia+Time_Surgery,  
    CEnd >= End.      

set_new_interval([(I1, _), (_, _), (_, F3)], RoomAgenda, Duration, NewIntervals) :-
    % Encontra todos os intervalos válidos na agenda
    findall((AEnd, Aux), (
        member((_, AEnd, _), RoomAgenda), % Percorre RoomAgenda
        I1 =< AEnd,                         % Início é maior ou igual ao limite inferior
        Aux is AEnd + Duration,             % Calcula o final do intervalo
        Aux =< F3                             % Garante que não ultrapassa o limite superior
    ), NewIntervals).                         % Retorna todos os intervalos válidos.


precede([], [], [], []).
precede([ (I1, F1) | L1 ], [ (I2, F2) | L2 ], [ (I3, F3) | L3 ], Result) :-
    I1 =< I2,
    F1 >=I2,
    I2=<I3,
    F2>=I3,
    precede(L1, L2, L3, Rest),
    Result = [ [(I1, F1), (I2, F2), (I3, F3)] | Rest ],
    !.
precede([ _ | L1 ], L2, L3, Result) :-
    precede(L1, L2, L3, Result).
precede(L1, [ _ | L2 ], L3, Result) :-
    precede(L1, L2, L3, Result).
precede(L1, L2, [ _ | L3 ], Result) :-
    precede(L1, L2, L3, Result).
precede([ _ | L1 ], [ _ | L2 ], [ _ | L3 ], Result) :-
    precede(L1, L2, L3, Result).