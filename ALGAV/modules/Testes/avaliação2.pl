:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_parameters)).
:- use_module(library(lists)).
:- use_module(library(http/http_cors)).
:- use_module(library(debug)). % Certifique-se de importar o módulo de depuração
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

% fetch_json(URL, JSON)
% Fetch JSON data from a given URL.
fetch_json(URL, JSON) :-
    http_open(URL, Stream, []),       % Open the URL and get the response stream
    json_read_dict(Stream, JSON),     % Parse the stream into a JSON dictionary
    close(Stream).                    % Close the stream after use


    :- set_prolog_flag(encoding, utf8).
    % Cors: Permitir requisições de qualquer origem
    :- set_setting(http:cors, [*]).

:- http_handler('/ola2', get_surgery_plan_response, [method(get)]).	

		
get_surgery_plan_response(_Request) :-
    cors_enable,
    (   search_pending_surgeries(20241216, '2') ->
       listasolucoes(A),
        findall(A, listasolucoes(A), AllSolutions),
        find_best_solution(AllSolutions, BestSolution),
        findall(X, (member((_, _, X), BestSolution), schedule_surgery(X, Date, Room, Solucoes1)), _),
        maplist(format_solution, BestSolution, FormattedSolucoes),


        reply_json_dict(_{
            status: "success",
            date: 20241216,
            room: "or1",
            solutions: FormattedSolucoes

        }, [encoding(utf8)])
    ; 
    reply_json_dict(_{
            status: "error",
            message: "Unable to compute solutions"
        })
    ).

format_solution((Start, End, SurgeryID), _{start_time: Start, end_time: End, surgery_id: SurgeryID}).


    
    % Servidor HTTP na porta 8080
    server(Port) :-
        http_server(http_dispatch, [port(Port)]).

    % Inicia o servidor automaticamente
    :- initialization(start_server).

    start_server :-
        server(8080).


:- dynamic availability/3.%:- retractall(availability(_,_,_)).
:-dynamic agenda_operation_room/3.
:-dynamic agenda_staff/3.
:- dynamic agenda_staff1/3.
:-dynamic agenda_staff2/3.
:-dynamic agenda_operation_room1/3.
:-dynamic assignment_surgery/2.
:-dynamic listasolucoes/1.
:-dynamic timetable/3.
:-dynamic staff/4.
:-dynamic surgery_id/2.
:-dynamic surgery/4.
:-dynamic surgery_staff_requirements/3.

%another example
agenda_staff(d001,20241216,[]).%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
agenda_staff(d002,20241216,[]).
agenda_staff(d003,20241216,[]).
agenda_staff(d004,20241216,[]).
agenda_staff(d005,20241216,[]).
agenda_staff(d006,20241216,[]).
agenda_staff(d007,20241216,[]).
agenda_staff(d008,20241216,[]).

timetable(d001,20241216,(480,1200)).
timetable(d002,20241216,(720,1440)).%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
timetable(d003,20241216,(600,1320)).
timetable(d004,20241216,(520,1320)).
timetable(d005,20241216,(520,1440)).
timetable(d006,20241216,(520,1440)).
timetable(d007,20241216,(520,1440)).
timetable(d008,20241216,(520,1440)).

%another example
%timetable(d001,20241216,(480,1200)).
%timetable(d002,20241216,(500,1440)).
%timetable(d003,20241216,(520,1320)).
%timetable(d004,20241216,(520,1320)).
%timetable(d005,20241216,(520,1320)).
%timetable(d006,20241216,(520,1320)).

staff(d001,doctor,orthopaedist,[so2,so3,so4]).
staff(d002,doctor,orthopaedist,[so2,so3,so4]).%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
staff(d003, doctor, anesthetist, [so2, so3]).
staff(d004, doctor, orthopaedist, [so2]).
staff(d005, doctor, cleaner, [so2]).
staff(d006, doctor, cleaner, [so2]).
staff(d007, doctor, anesthetist, [so2, so3]).
staff(d008, doctor, anesthetist, [so2, so3]).

surgery_staff_requirements(operation_team,so2, [(orthopaedist, 2)]).
surgery_staff_requirements(anesthetist_team,so2, [(anesthetist, 2)]).
surgery_staff_requirements(cleaning_team,so2, [(cleaner, 1)]).

surgery_staff_requirements(operation_team,so3, [(orthopaedist, 1)]).
surgery_staff_requirements(anesthetist_team,so3, [(anesthetist, 1)]).
surgery_staff_requirements(cleaning_team,so3, [(cleaner, 1)]).

surgery_staff_requirements(operation_team,so4, [(orthopaedist, 1)]).
surgery_staff_requirements(anesthetist_team,so4, [(anesthetist, 1)]).
surgery_staff_requirements(cleaning_team,so4, [(cleaner, 1)]).

agenda_operation_room(or1,20241216,[(520,579,so100000)]).
agenda_operation_room(ola,20241216,[]).%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

surgery(so2,45,60,45).
surgery(so3,45,90,45).
surgery(so4,45,75,45).%%%%%%%%%%%%%%%%%%%%%%%%%

surgery_id(so100001,so2).
surgery_id(so100002,so3).
surgery_id(so100003,so4).%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
surgery_id(so100004,so2).
surgery_id(so100005,so4).



find_free_agendas(Date):-
    retractall(availability(_,_,_)),
    findall(_,(agenda_staff(D,Date,L),free_agenda_staff0(L,LFA),adapt_timetable(D,Date,LFA,LFA2),assertz(availability(D,Date,LFA2))),_).

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
intersect_2_agendas([D|LD],LA,LIT):-	
    intersect_availability(D,LA,LI,LA1),
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

find_best_solution(Solutions, BestSolution) :-
    find_best_solution(Solutions, [], 1441, BestSolution).

find_best_solution([], CurrentBest, _, CurrentBest).

find_best_solution([Solution|Rest], CurrentBest, CurrentMinTime, BestSolution) :-
    evaluate_final_time(Solution, TFin),
    (TFin < CurrentMinTime ->
        NewBest = Solution,
        NewMinTime = TFin
    ;
        NewBest = CurrentBest,
        NewMinTime = CurrentMinTime
    ),
    find_best_solution(Rest, NewBest, NewMinTime, BestSolution).

evaluate_final_time(Solution, TFin) :-
    last(Solution, (_, TFin, _)).  % Pega o tempo final da última tupla da solução

search_pending_surgeries(Date, Room) :-
    findall(_, (agenda_staff(D, Day, Agenda), assertz(agenda_staff2(D, Day, Agenda))), _),
    format('ola'),
    agenda_operation_room(Room, Date, Agenda),
    format('ola'),

    retractall(agenda_operation_room1(Room, Date, _)),
    assertz(agenda_operation_room1(Room, Date, Agenda)),
    findall(Surgery, surgery_id(Surgery, _), SurgeryList),
    (
        permutation(SurgeryList, PermutedList),
        % Certifique-se de que PermutedList é seguro para exibição
        schedule_pending_surgeries(PermutedList, Date, Room, Solucoes),
        fail
    ;
        true
    ).

schedule_pending_surgeries([], Date, Room,Solucoes) :-

    retractall(agenda_staff(D,Dia,_)),    
    findall(_,(agenda_staff2(D,Day,Agenda),assertz(agenda_staff(D,Day,Agenda))),_),
    agenda_operation_room1(Room, Date, Agenda),
    retractall(agenda_operation_room(Room, Date, _)),
    assertz(agenda_operation_room(Room, Date, Agenda)),
    assertz(listasolucoes(Solucoes)).
    
schedule_pending_surgeries([X | Rest], Date, Room,Solucoes) :-
    (   
        schedule_surgery(X, Date, Room,Solucoes1) ->
        true
    ;  
    true
    ),
    schedule_pending_surgeries(Rest, Date, Room,Solucoes1).


schedule_surgery(Surgery, Date, Room,Solucoes) :-
    surgery_id(Surgery, Tipo),
    surgery(Tipo,Time_Anesthesia,Time_Surgery,Time_Cleaning),
    Duration is Time_Anesthesia + Time_Surgery + Time_Cleaning,
    surgery_staff_requirements(operation_team,Tipo, Requirements),
    surgery_staff_requirements(anesthetist_team,Tipo,Requirements2),
    surgery_staff_requirements(cleaning_team,Tipo, Requirements3),
    find_free_agendas(Date),
    get_staff_for_surgery(Requirements, StaffList),
    get_staff_for_surgery(Requirements2, Staff_AnesthesiaList),
    get_staff_for_surgery(Requirements3, Staff_CleaningList),
    findall(CommonIntervals, (member(Team, StaffList), intersect_all_agendas(Team, Date, CommonIntervals)), AllCommonIntervals),   
    findall(CommonIntervals, (member(Team, Staff_AnesthesiaList), intersect_all_agendas(Team, Date, CommonIntervals)), AllCommonIntervals_Anesthesia),
    findall(CommonIntervals, (member(Team, Staff_CleaningList), intersect_all_agendas(Team, Date, CommonIntervals)), AllCommonIntervals_Cleaning),
    findall(SurgeryInterval, (member(CommonIntervals, AllCommonIntervals), select_sufficient_interval(CommonIntervals, Time_Surgery, SurgeryInterval)), AllSurgeryInterval),
    flatten(AllSurgeryInterval, Result_surgery),
    list_to_set(Result_surgery, UniqueResultsurgery),
    findall(SurgeryInterval, (member(CommonIntervals, AllCommonIntervals_Anesthesia), select_sufficient_interval(CommonIntervals, Time_Anesthesia, SurgeryInterval)), AllSurgeryInterval_Anesthesia),
    flatten(AllSurgeryInterval_Anesthesia, Result_Anesthesia),
    list_to_set(Result_Anesthesia, UniqueResultAnesthesia),
    findall(SurgeryInterval, (member(CommonIntervals, AllCommonIntervals_Cleaning), select_sufficient_interval(CommonIntervals, Time_Cleaning, SurgeryInterval)), AllSurgeryInterval_Cleaning),
    flatten(AllSurgeryInterval_Cleaning, Result_Cleaning),
    list_to_set(Result_Cleaning, UniqueResultCleaning),
    precede(UniqueResultAnesthesia,UniqueResultsurgery,UniqueResultCleaning,Result_allCombinatios),
    list_to_set(Result_allCombinatios, Result_allCombinatios1),
    agenda_operation_room(Room, Date, RoomAgenda),
    findall(NewInterval, (
    member(SurgeryInterval1, Result_allCombinatios),
    set_new_interval(SurgeryInterval1,RoomAgenda,Duration,NewInterval)), StaffRoomIntervals),
    flatten(StaffRoomIntervals, StaffRoomIntervals1),
    list_to_set(StaffRoomIntervals1, StaffRoomIntervals2),
    findall(SurgeryInterval, (member(SurgeryInterval, StaffRoomIntervals2), check_room_availability(Room, Date, SurgeryInterval)), ValidRoomIntervals),
    min_final_minute(ValidRoomIntervals, MinInterval, UpdatedList),

assign_surgery2(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval,Staff_AnesthesiaList,ValidRoomIntervals,Staff_CleaningList,StaffList,Solucoes).


       assign_surgery2(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval,Staff_AnesthesiaList,ValidRoomIntervals,Staff_CleaningList,StaffList,Solucoes):-
(      assign_surgery(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval,Staff_AnesthesiaList,Staff_CleaningList,StaffList,Solucoes)
    -> true
    ;
        min_final_minute(ValidRoomIntervals, MinInterval1, UpdatedList),
        assign_surgery2(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval1,Staff_AnesthesiaList,UpdatedList,Staff_CleaningList,StaffList,Solucoes)   
).

assign_surgery(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,MinInterval,Staff_AnesthesiaList,Staff_CleaningList,StaffList,Solucoes) :-    

    find_staff_with_min_interval(StaffList, Date, MinInterval, StaffWithMinInterval, Tipo),
    find_staff_with_min_intervalAnesthesia(Staff_AnesthesiaList, Date, MinInterval, StaffWithMinInterval_AnesthesiaList, Tipo),
    find_staff_with_min_intervalClenaing(Staff_CleaningList, Date, MinInterval, StaffWithMinInterval_CleaningLis, Tipo),
    [SelectedTeam | _] = StaffWithMinInterval,
    [SelectedTeamAnesthesia | _] = StaffWithMinInterval_AnesthesiaList,
    [SelectedTeamCleaning | _] = StaffWithMinInterval_CleaningLis, 
    add_assignment_surgery(Surgery, Room),
    (Start, End) = MinInterval,
    AdjustedStartSurgery is Start + Time_Anesthesia,  
    AdjustedEndSurgery is End - Time_Cleaning,        
    AdjustedStartAnesthesia is Start,  
    AdjustedEndAnesthesia is End - Time_Cleaning,        
    AdjustedStartCleaning is Start + Time_Anesthesia + Time_Surgery,  
    AdjustedEndCleaning is End,        
    update_staff_agendas(SelectedTeam, Date, (AdjustedStartSurgery, AdjustedEndSurgery), Surgery),   
    update_staff_agendas(SelectedTeamAnesthesia, Date, (AdjustedStartAnesthesia, AdjustedEndAnesthesia), Surgery),   
    update_staff_agendas(SelectedTeamCleaning, Date, (AdjustedStartCleaning, AdjustedEndCleaning), Surgery),   
    update_room_agenda(Room, Date, MinInterval, Surgery,Solucoes).
    
assign_surgery(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,Solucoes) :-
    assign_surgery(Room, Date, Surgery, Time_Anesthesia, Time_Cleaning, Time_Surgery, Tipo,Solucoes).

get_staff_for_surgery([], [[]]).
get_staff_for_surgery([(Type, Quantity) | RestRequirements], AllStaffLists) :-
    findall(ID, staff(ID, _, Type, _), AllStaff), 
    findall(Selected, (combination(Quantity, AllStaff, Selected)), PossibleSelections),
    get_staff_for_surgery(RestRequirements, RestStaffLists), 
    findall(AppendedList,
        (member(Selected, PossibleSelections),
         member(RestStaff, RestStaffLists), 
         append(Selected, RestStaff, AppendedList)), 
         AllStaffLists). 

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
    Start2 < Start)).
         
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

update_room_agenda(Room, Date, (Start, End), Surgery,Solucoes) :-
    agenda_operation_room(Room, Date, CurrentAgenda),

    append(CurrentAgenda, [(Start, End, Surgery)], UpdatedAgenda),
    Solucoes=UpdatedAgenda,
    append(CurrentAgenda, [(Start, End, Surgery)], Solucoes),

    retractall(agenda_operation_room(Room, Date, _)),
    assertz(agenda_operation_room(Room, Date, UpdatedAgenda)).

add_assignment_surgery(SurgeryID, Room) :-
    assertz(assignment_surgery(SurgeryID, Room)).
   
min_final_minute(AllCommonIntervals, MinInterval,NewList_Unique) :-
    flatten(AllCommonIntervals, Flattened),
    findall((Start, End), member((Start, End), Flattened), Intervals), 
    findall(End, member((_, End), Intervals), EndMinutes), 
    min_list(EndMinutes, MinMinute), % menor minuto final
    member(MinInterval1, Intervals), 
    MinInterval1 = (_, MinMinute),
    select(MinInterval1, AllCommonIntervals, NewList_Unique),
     select(MinInterval, AllCommonIntervals, NewList_Unique).

find_staff_with_min_interval(StaffList, Date, MinInterval, StaffWithMinInterval,Tipo) :-
    findall(Team, (
        member(Team, StaffList),
        intersect_all_agendas(Team, Date, CommonIntervals),
        is_available_in_intervals(MinInterval, CommonIntervals,Tipo)
    ), StaffWithMinInterval).
    
 find_staff_with_min_intervalClenaing(StaffList, Date, MinInterval, StaffWithMinInterval,Tipo) :-
     findall(Team, (
         member(Team, StaffList),
         intersect_all_agendas(Team, Date, CommonIntervals),
         is_available_in_intervalsClenaing(MinInterval, CommonIntervals,Tipo)
     ), StaffWithMinInterval).
     
find_staff_with_min_intervalAnesthesia(StaffList, Date, MinInterval, StaffWithMinInterval,Tipo) :-
    findall(Team, (
        member(Team, StaffList),
        intersect_all_agendas(Team, Date, CommonIntervals),
        is_available_in_intervalsAnesthesia(MinInterval, CommonIntervals,Tipo)
    ), StaffWithMinInterval).

is_available_in_intervals((Start, End), CommonIntervals,Tipo) :-
    surgery(Tipo,Time_Anesthesia,_,Time_Cleaning),
    member((CStart, CEnd), CommonIntervals),
    CStart =< Start + Time_Anesthesia, 
    CEnd >= End - Time_Cleaning. 

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
    findall((AEndAuxIncrement, AuxIncrement), (
        member((_, AEnd, _), RoomAgenda), 
        I1 =< AEnd,                      
        Aux is AEnd + 1 + Duration, 
        AEndAux is AEnd + 1,
        Aux =< F3,
        between(0, 266, Step),            
        AEndAuxIncrement is AEndAux + Step, 
        AuxIncrement is Aux + Step        
    ), IntervalsFromEnd),
    findall((AEndAuxIncrement, AuxIncrement), (
        member((_, AEnd, _), RoomAgenda), 
        I1 > AEnd,                      
        Aux is I1 + Duration, 
        Aux =< F3,
        between(0, 266, Step),            
        AEndAuxIncrement is I1 + Step, 
        AuxIncrement is Aux + Step                         
        ), IntervalsFromEnd2),
    findall((AEndAuxIncrement, AuxIncrement), (
        member((AStart, _, _), RoomAgenda), 
        I1 =< AStart,                         
        Aux is I1 + Duration,             
        Aux =< F3,        
        between(0, 266, Step),            
        AEndAuxIncrement is I1 + Step, 
        AuxIncrement is Aux + Step                            
        ), IntervalsFromStart) ,                        
    append(IntervalsFromEnd, IntervalsFromEnd2, NewIntervals0),
    append(NewIntervals0, IntervalsFromStart, NewIntervals).

precede([], [], [], []).

% Gera combinações válidas
precede(L1, L2, L3, Result) :-
    findall([X, Y, Z], (
        member(X, L1),  
        member(Y, L2),  
        member(Z, L3),  
        X = (I1, F1),          
        Y = (I2, F2),           
        Z = (I3, _),           
        I1 =< I2,              
        F1 >= I2,               
        I2 =< I3,               
        F2 >= I3                
       ), Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- http_handler('/load', load_data_handler, []).	
load_data_handler(_Request) :-
cors_enable,
reload_all_data,
reply_json_dict(_{status: "success"}).      

reload_all_data :-
reload_surgery_rooms_data,
reload_staff_data,
reload_operation_requests,
reload_operation_types,
!. 


reload_surgery_rooms_data :-
clear_surgery_rooms,
!,
load_surgery_rooms.
clear_surgery_rooms :-
retractall(agenda_operation_room(_, _, _)),
!.


reload_operation_requests :-
    clear_operation_requests,
    !,
    load_operation_requests.
clear_operation_requests :-
    retractall(operation_request(_, _, _, _, _, _)),
    retractall(surgery_id(_, _)),
    !.


reload_operation_types :-
    clear_operation_types,
    !,
    load_operation_types.
    
% Clear operation-type data
clear_operation_types :-
    retractall(surgery(_, _, _, _)),
    %TODO:REVERretractall(surgery_staff_requirements(_, _, _)),
    !.    
            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definir predicados dinâmicos
:- dynamic room_agenda/3.
process_rooms([]) :- !.  % Caso base
process_rooms([Room | Rest]) :-
    % Extrair campos principais
    string_to_atom_safe(Room.id, RoomID),
    string_to_atom_safe(Room.type, RoomType),
    string_to_atom_safe(Room.status, RoomStatus),
    Room.capacity.capacity = Capacity,
    Room.equipment = Equipment,

    % Processar agenda associada à sala
    RoomAgenda = Room.roomAgenda,
    process_room_agenda(RoomID, RoomAgenda),
    !,
    process_rooms(Rest).

process_room_agenda(_, []) :- !. 
process_room_agenda(RoomID, [AgendaEntry | Rest]) :-
    string_to_atom_safe(AgendaEntry.date, DateISO),
    date_iso_to_ymd(DateISO, FormattedDate),
    Intervals = AgendaEntry.timeIntervals,
      AppointmentType = AgendaEntry.appointmentType,
       collect_intervals(Intervals, [], CollectedIntervals,AppointmentType), % Agregar os intervalos     

    assertz(agenda_operation_room(RoomID, FormattedDate, CollectedIntervals)),
       !,
    process_room_agenda(RoomID, Rest).

time_to_minutes(TimeString, Minutes) :-
    split_string(TimeString, ":", "", [HourStr, MinStr]),
    number_string(Hours, HourStr),
    number_string(MinutesPart, MinStr),
    Minutes is Hours * 60 + MinutesPart.

load_surgery_rooms :-
    fetch_json('http://localhost:5001/algav/surgery-room', RoomsJSON),
    process_rooms(RoomsJSON).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reload_staff_data :-
    clear_staff,
    !,
    load_staff.

clear_staff :-
    retractall(staff(_, _, _, _)),
    retractall(agenda_staff(_, _, _)),
    !.

load_staff :-
    % Obter os dados da equipe (substitua pela URL ou caminho correto)
    fetch_json('http://localhost:5001/algav/staff', StaffJSON),
    
    % Processar os dados da equipe
    process_staff(StaffJSON).

% process_staff/1
% Processa a lista completa de membros da equipe.
process_staff([]) :- !.  % Caso base
process_staff([Staff | Rest]) :-
    % Extrair campos principais
    string_to_atom_safe(Staff.id.value, StaffID),
    string_to_atom_safe(Staff.licenseNumber, LicenseNumber),
    string_to_atom_safe(Staff.specialization.specializationName.name, Specialization),
    string_to_atom_safe(Staff.status, Status),
    AvailabilitySlots = Staff.availabilitySlots,
    process_availability_slots(StaffID, AvailabilitySlots),

    % Armazenar os fatos do staff
   assertz(staff(StaffID, doctor, Specialization, Status)),

    StaffAgenda = Staff.staffAgenda,
    process_staff_agenda_entries(StaffID, StaffAgenda),
    !,
    process_staff(Rest).

% Predicado auxiliar para processar os AvailabilitySlots
process_availability_slots(_, []) :- !.  % Caso base: lista vazia
process_availability_slots(StaffID, [Slot | Rest]) :-
    split_string(Slot, "-", "", [DateStr, StartStr, EndStr]),
    atom_number(DateStr, Date),               % Converter data para número
    time_to_minutes(StartStr, StartMinutes),  % Converter hora inicial para minutos
    time_to_minutes(EndStr, EndMinutes),      % Converter hora final para minutos

    assertz(timetable(StaffID, Date, (StartMinutes, EndMinutes))),

    process_availability_slots(StaffID, Rest).


% process_staff_agenda_entries/2
% Processa as entradas de agenda para um membro específico do staff.
process_staff_agenda_entries(_, []) :- !.  % Caso base
process_staff_agenda_entries(StaffID, [AgendaEntry | Rest]) :-
    % Extrair data e intervalos da agenda
    string_to_atom_safe(AgendaEntry.date, DateISO),
    date_iso_to_ymd(DateISO, FormattedDate),  % Converter ISO para formato YYYYMMDD
    Intervals = AgendaEntry.timeIntervals,
    AppointmentType = AgendaEntry.appointmentType,
    collect_intervals(Intervals, [], CollectedIntervals,AppointmentType), % Agregar os intervalos
     
    assertz(agenda_staff(StaffID, FormattedDate, CollectedIntervals)),
        !,
    process_staff_agenda_entries(StaffID, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_operation_requests([]):-!.
process_operation_requests([Request | Rest]) :-
    % Extract fields safely
    string_to_atom_safe(Request.get(id), ID),
    Deadline = Request.get(deadline),
    (   Request.get(doctorName) == null 
    ->  DoctorName = null
    ;   string_to_atom_safe(Request.get(doctorName), DoctorName)
    ),
    (   Request.get(operationType) == null 
    ->  OperationType = null
    ;   OperationType=Request.get(operationType)

    ),
    (   Request.get(patientName) == null 
    ->  PatientName = null
    ;   string_to_atom_safe(Request.get(patientName), PatientName)
    ),
    string_to_atom_safe(Request.get(priority), Priority),
    
    assertz(operation_request(ID, Deadline, DoctorName, OperationType, PatientName, Priority)),
    assertz(surgery_id(ID,OperationType)),
           !,
    process_operation_requests(Rest).

% Main loader predicate
load_operation_requests :-
    fetch_json('http://localhost:5001/algav/operation-request', OperationRequestsJSON),
    process_operation_requests(OperationRequestsJSON).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_operation_types([]):- !.
process_operation_types([Operation | Rest]) :-
    % Extract fields
    atom_string_safe(Operation.get(id), OperationID),
    Name = Operation.get(name), % Extract the Name correctly
    time_to_minutes(Operation.get(cleaningDuration), CleaningDuration),
    time_to_minutes(Operation.get(surgeryDuration), SurgeryDuration),
    time_to_minutes(Operation.get(setupDuration), SetupDuration),
    StillPerformed = Operation.get(stillPerformed),
    (   StillPerformed == null
    ->  StillPerformedFlag = false
    ;   StillPerformedFlag = StillPerformed
    ),
    RequiredStaffList = Operation.get(requiredStaff), % Extract the list of required staff

    assertz(surgery(Name, SetupDuration, SurgeryDuration, CleaningDuration)),
    %TODO:REVERprocess_required_staff(OperationID, RequiredStaffList),
    !,
    process_operation_types(Rest).

% Process the required staff list for an operation
process_required_staff(_, null) :- !. % Case where there is no required staff (null)
process_required_staff(OperationID, []) :- !. % Base case for empty list
process_required_staff(OperationID, [Staff | Rest]) :-
    atom_string_safe(Staff.get(id), ID),
    NumberOfStaff = Staff.numberOfStaff.get(value),
    (   Staff.specialization == null
    ->  Specialization = null
    ;   atom_string_safe(Staff.get(specialization), Specialization)
    ),
    
    % Assert the required_staff fact
    assertz(surgery_staff_requirements(ID, NumberOfStaff, Specialization)),
    assertz(required_staff(ID, NumberOfStaff, Specialization)),
    !,
    process_required_staff(OperationID, Rest). % Process the rest of the list

% Main loader predicate
load_operation_types :-
    fetch_json('http://localhost:5001/algav/operation-type', OperationTypesJSON),
    process_operation_types(OperationTypesJSON).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
collect_intervals([], Acc, Acc,_) :- !. 
collect_intervals([Interval | Rest], Acc, FinalList,AppointmentType) :-
    split_string(Interval, "-", "", [StartStr, EndStr]),

    % Converte as horas para minutos
    time_to_minutes(StartStr, MinInicial),
    time_to_minutes(EndStr, MinFinal),

    % Adiciona o intervalo à lista acumulada
    append(Acc, [(MinInicial, MinFinal,AppointmentType)], UpdatedAcc),

    % Processa o restante dos intervalos
    collect_intervals(Rest, UpdatedAcc, FinalList,AppointmentType).
    
date_iso_to_ymd(DateISO, FormattedDate) :-
    split_string(DateISO, "T", "", [DatePart | _]),
    split_string(DatePart, "-", "", [Year, Month, Day]),
    atomic_list_concat([Year, Month, Day], '', FormattedDate).


% Converte strings para átomos com segurança
string_to_atom_safe(Value, Atom) :-
    (   var(Value) -> Atom = 'Unknown'  % Caso a variável seja indefinida
    ;   string(Value) -> atom_string(Atom, Value)  % Caso seja uma string válida
    ;   atom(Value) -> Atom = Value  % Já é um átomo
    ;   number(Value) -> atom_number(Atom, Value)  % Caso seja um número
    ;   is_dict(Value) -> Atom = 'InvalidInput'  % Caso seja um dicionário
    ;   Atom = 'Unknown'  % Fallback para entradas não reconhecidas
    ).
string_to_atom_safe(String, Atom) :-
    ( var(String) -> Atom = 'Unknown' ; string_to_atom(String, Atom) ).

% Utility: Time conversion
time_to_minutes(TimeString, Minutes) :-
    (   TimeString == null
    ->  Minutes = 0 % Handle null time values gracefully
    ;   split_string(TimeString, ":", "", [HStr, MStr, _]),
        number_string(Hours, HStr),
        number_string(MinutesPart, MStr),
        TotalMinutes is Hours * 60 + MinutesPart,
        Minutes is TotalMinutes
    ).

atom_string_safe(String, Atom) :-
    (   string(String)
    ->  atom_string(Atom, String)
    ;   Atom = String
    ).