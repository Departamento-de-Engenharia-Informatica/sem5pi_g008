:- dynamic surgery/4.
:- dynamic staff/4.
:- dynamic timetable/3.
:- dynamic surgery_id/2.
:- dynamic agenda_staff/3.
:- dynamic agenda_operation_room/3.
:- dynamic surgery_staff_requirements/3.

:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

% fetch_json(URL, JSON)
% Fetch JSON data from a given URL.
fetch_json(URL, JSON) :-
    http_open(URL, Stream, []),       % Open the URL and get the response stream
    json_read_dict(Stream, JSON),     % Parse the stream into a JSON dictionary
    close(Stream).                    % Close the stream after use



% Load all data


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process and load operation request data
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
    ;   string_to_atom_safe(Request.get(operationType), OperationType)
    ),
    (   Request.get(patientName) == null 
    ->  PatientName = null
    ;   string_to_atom_safe(Request.get(patientName), PatientName)
    ),
    string_to_atom_safe(Request.get(priority), Priority),
    
    % Assert operation_request fact
    assertz(operation_request(ID, Deadline, DoctorName, OperationType, PatientName, Priority)),
    assertz(surgery_id(ID,OperationType)),
    format('surgery id ~w: ~w\n',[ID,OperationType]),
    % Print confirmation for debugging
    format('Operation Request ~w: Deadline: ~w, Doctor: ~w, Operation Type: ~w, Patient: ~w, Priority: ~w\n',
           [ID, Deadline, DoctorName, OperationType, PatientName, Priority]),
           !,
    process_operation_requests(Rest).

% Main loader predicate
load_operation_requests :-
    fetch_json('http://localhost:5001/algav/operation-request', OperationRequestsJSON),
    process_operation_requests(OperationRequestsJSON).

% Clear all operation request data
clear_operation_requests :-
    retractall(operation_request(_, _, _, _, _, _)),
    write('Operation request data cleared.\n').

% Converte strings para átomos com segurança
string_to_atom_safe(Value, Atom) :-
    (   var(Value) -> Atom = 'Unknown'  % Caso a variável seja indefinida
    ;   string(Value) -> atom_string(Atom, Value)  % Caso seja uma string válida
    ;   atom(Value) -> Atom = Value  % Já é um átomo
    ;   number(Value) -> atom_number(Atom, Value)  % Caso seja um número
    ;   is_dict(Value) -> Atom = 'InvalidInput'  % Caso seja um dicionário
    ;   Atom = 'Unknown'  % Fallback para entradas não reconhecidas
    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process and load operation-type data
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
    % Assert the operation-type fact
    assertz(operation_type(OperationID, SetupDuration, SurgeryDuration, CleaningDuration, StillPerformedFlag)),
    assertz(surgery(Name, SetupDuration, SurgeryDuration, CleaningDuration)),
    % Print confirmation
    format('\n\nOperation ~w: Setup: ~w min, Surgery: ~w min, Cleaning: ~w min, Still Performed: ~w, Name: ~w\n',
           [OperationID, SetupDuration, SurgeryDuration, CleaningDuration, StillPerformedFlag, Name]),    process_required_staff(OperationID, RequiredStaffList),

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
    % Print confirmation
    format('Required Staff ~w:\n Number of Staff: ~w, \nSpecialization: ~w\n',
           [ID, NumberOfStaff, Specialization]),
    !,
    process_required_staff(OperationID, Rest). % Process the rest of the list

% Main loader predicate
load_operation_types :-
    fetch_json('http://localhost:5001/algav/operation-type', OperationTypesJSON),
    process_operation_types(OperationTypesJSON).

% Clear all operation-type data
clear_operation_types :-
    retractall(operation_type(_, _, _, _, _)),
    write('Operation type data cleared.\n').

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
    
% Utility: Convert string to atom safely (equivalent to atom_string/2)
atom_string_safe(String, Atom) :-
    (   string(String)
    ->  atom_string(Atom, String)
    ;   Atom = String
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definir predicados dinâmicos
:- dynamic surgery_room/5.
:- dynamic room_agenda/3.

% process_rooms/1
% Processa a lista completa de salas de cirurgia.
process_rooms([]) :- !.  % Caso base
process_rooms([Room | Rest]) :-
    % Extrair campos principais
    string_to_atom_safe(Room.id, RoomID),
    string_to_atom_safe(Room.type, RoomType),
    string_to_atom_safe(Room.status, RoomStatus),
    Room.capacity.capacity = Capacity,
    Room.equipment = Equipment,

    % Armazenar o fato da sala
    assertz(surgery_room(RoomID, RoomType, Capacity, Equipment, RoomStatus)),

    % Processar agenda associada à sala
    RoomAgenda = Room.roomAgenda,
    process_room_agenda(RoomID, RoomAgenda),

    % Exibir confirmação
    format('Room ~w: Type ~w, Capacity ~w, Status ~w, Equipment ~w\n',
           [RoomID, RoomType, Capacity, RoomStatus, Equipment]),
    !,
    process_rooms(Rest).

% process_room_agenda/2
% Processa a agenda associada a uma sala específica.
process_room_agenda(_, []) :- !.  % Caso base
process_room_agenda(RoomID, [AgendaEntry | Rest]) :-
    % Extrair data e intervalos da agenda
    string_to_atom_safe(AgendaEntry.date, DateISO),
    date_iso_to_ymd(DateISO, FormattedDate),
    Intervals = AgendaEntry.timeIntervals,
      AppointmentType = AgendaEntry.appointmentType,
       collect_intervals(Intervals, [], CollectedIntervals,AppointmentType), % Agregar os intervalos     

    assertz(room_agenda(RoomID, FormattedDate, CollectedIntervals)),

    % Exibir confirmação para depuração
       format('Staff Agenda Consolidated: StaffID ~w, Date ~w, Intervals ~w\n',
              [RoomID, FormattedDate, CollectedIntervals]),
       !,
    process_room_agenda(RoomID, Rest).

% time_to_minutes/2
% Converte um horário no formato 'HH:MM' para minutos totais.
time_to_minutes(TimeString, Minutes) :-
    split_string(TimeString, ":", "", [HourStr, MinStr]),
    number_string(Hours, HourStr),
    number_string(MinutesPart, MinStr),
    Minutes is Hours * 60 + MinutesPart.

% Converter datas ISO (ex: "2024-12-15T00:00:00") para formato YYYYMMDD.
date_iso_to_ymd(DateISO, FormattedDate) :-
    split_string(DateISO, "T", "", [DatePart | _]),
    split_string(DatePart, "-", "", [Year, Month, Day]),
    atomic_list_concat([Year, Month, Day], '', FormattedDate).

% Função auxiliar para converter strings em átomos com segurança.
string_to_atom_safe(String, Atom) :-
    ( var(String) -> Atom = 'Unknown' ; string_to_atom(String, Atom) ).

% Predicado principal para carregar os dados JSON e processar
load_surgery_rooms :-
    fetch_json('http://localhost:5001/algav/surgery-room', RoomsJSON),
    process_rooms(RoomsJSON).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Process and load required staff data
process_required_staff([]):-!.
process_required_staff([Staff | Rest]) :-
    % Extract fields
    string_to_atom_safe(Staff.get(id), ID),
    NumberOfStaff = Staff.numberOfStaff.get(value),
    (   Staff.specialization == null 
    ->  Specialization = null
    ;   string_to_atom_safe(Staff.get(specialization), Specialization)
    ),
    % Assert the required_staff fact
    assertz(surgery_staff_requirements(ID, NumberOfStaff , Specialization)),
    assertz(required_staff(ID, NumberOfStaff, Specialization)),
    % Print confirmation
    format('Required Staff ~w: Number of Staff: ~w, Specialization: ~w\n',
           [ID, NumberOfStaff, Specialization]),!,
    process_required_staff(Rest).

% Main loader predicate
load_required_staff :-
    fetch_json('http://localhost:5001/algav/RequiredStaff', RequiredStaffJSON),
    process_required_staff(RequiredStaffJSON).

% Clear all required staff data
clear_required_staff :-
    retractall(required_staff(_, _, _)),
    write('Required staff data cleared.\n').

% Utility: Safe conversion of string to atom

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Declarar os fatos dinâmicos
:- dynamic staff/4.
:- dynamic staff_agenda/3.

% load_staff/0
% Carrega e processa os dados de membros da equipe.
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
    assertz(staff(StaffID, LicenseNumber, Specialization, Status)),

    % Processar agenda associada ao staff
    StaffAgenda = Staff.staffAgenda,
    process_staff_agenda_entries(StaffID, StaffAgenda),

    % Exibir confirmação para depuração
    format('Staff ~w: License ~w, Specialization ~w, Status ~w\n',
           [StaffID, LicenseNumber, Specialization, Status]),
    !,
    process_staff(Rest).

% Predicado auxiliar para processar os AvailabilitySlots
process_availability_slots(_, []) :- !.  % Caso base: lista vazia
process_availability_slots(StaffID, [Slot | Rest]) :-
    % Dividir a string Slot no formato "20241028-09:00-10:00"
    split_string(Slot, "-", "", [DateStr, StartStr, EndStr]),
    atom_number(DateStr, Date),               % Converter data para número
    time_to_minutes(StartStr, StartMinutes),  % Converter hora inicial para minutos
    time_to_minutes(EndStr, EndMinutes),      % Converter hora final para minutos

    % Armazenar o fato timetable
    assertz(timetable(StaffID, Date, (StartMinutes, EndMinutes))),

    % Mensagem de depuração
    format('Timetable added: ~w, Date: ~w, Interval: (~w, ~w)\n',
           [StaffID, Date, StartMinutes, EndMinutes]),

    % Processar o restante da lista de slots
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
     
  % Armazenar os intervalos coletados em um fato
    assertz(agenda_staff(StaffID, FormattedDate, CollectedIntervals)),
    
     % Exibir confirmação para depuração
        format('Staff Agenda Consolidated: StaffID ~w, Date ~w, Intervals ~w\n',
               [StaffID, FormattedDate, CollectedIntervals]),
        !,
    process_staff_agenda_entries(StaffID, Rest).

% collect_intervals/3
% Acumula os intervalos convertidos para minutos em uma lista.
collect_intervals([], Acc, Acc,_) :- !.  % Caso base: a lista está completa
collect_intervals([Interval | Rest], Acc, FinalList,AppointmentType) :-
    % Divide o intervalo no formato 'HH:MM-HH:MM' em partes
    split_string(Interval, "-", "", [StartStr, EndStr]),

    % Converte as horas para minutos
    time_to_minutes(StartStr, MinInicial),
    time_to_minutes(EndStr, MinFinal),

    % Adiciona o intervalo à lista acumulada
    append(Acc, [(MinInicial, MinFinal,AppointmentType)], UpdatedAcc),

    % Processa o restante dos intervalos
    collect_intervals(Rest, UpdatedAcc, FinalList,AppointmentType).


% Converter datas ISO (ex: "2024-12-15T00:00:00") para formato YYYYMMDD.
date_iso_to_ymd(DateISO, FormattedDate) :-
    split_string(DateISO, "T", "", [DatePart | _]),
    split_string(DatePart, "-", "", [Year, Month, Day]),
    atomic_list_concat([Year, Month, Day], '', FormattedDate).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Reload all operation request data
reload_operation_requests :-
    clear_operation_requests,
    !,
    load_operation_requests.

% Reload all operation-type data
reload_operation_types :-
    clear_operation_types,
    !,
    load_operation_types.

% Reload all surgery rooms data
reload_surgery_rooms_data :-
    clear_surgery_rooms,
    !,
    load_surgery_rooms.

% Clear surgery rooms data (helper for reload)
clear_surgery_rooms :-
    retractall(surgery_room(_, _, _, _, _)),
    retractall(room_agenda(_, _, _)),
    write('Surgery rooms data cleared.\n'),
    !.

% Reload all required staff data
reload_required_staff_data :-
    clear_required_staff,
    !,
    load_required_staff.

% Reload staff data and agenda
reload_staff_data :-
    clear_staff,
    !,
    load_staff.

% Clear staff data (helper for reload)
clear_staff :-
    retractall(staff(_, _, _, _)),
    retractall(staff_agenda(_, _, _)),
    write('Staff data cleared.\n'),
    !.

% Reload all data
reload_all_data :-
 %   reload_operation_requests,
    reload_operation_types,
  %   reload_surgery_rooms_data,
    % reload_required_staff_data,
    reload_staff_data,
    write('All data reloaded successfully.\n'),
    !.

% Clear operation request data
clear_operation_requests :-
    retractall(operation_request(_, _, _, _, _, _)),
    retractall(surgery_id(_, _)),
    write('Operation request data cleared.\n'),
    !.

% Clear operation-type data
clear_operation_types :-
    retractall(operation_type(_, _, _, _, _)),
    retractall(surgery(_, _, _, _)),
    write('Operation type data cleared.\n'),
    !.

% Clear required staff data
clear_required_staff :-
    retractall(required_staff(_, _, _)),
    retractall(surgery_staff_requirements(_, _, _)),
    write('Required staff data cleared.\n'),
    !.
