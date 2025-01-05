% Declaração dos predicados dinâmicos
:- dynamic surgery/4.
:- dynamic staff/4.
:- dynamic timetable/3.
:- dynamic surgery_id/2.
:- dynamic agenda_staff/3.
:- dynamic agenda_operation_room/3.
:- dynamic assignment_surgery/2.

% Load all data including agenda_staff
load_data :- 
    load_surgeries_data,
    load_staff_data,
    load_timetable_data,
    load_surgeryId_data,
    load_agenda_staff_data,
    load_agenda_operation_room_data,
    load_assignment_surgery_data.
   


% Convert time in "HH:MM:SS" format to minutes (including seconds)
time_to_minutes(TimeString, Minutes) :-
    split_string(TimeString, ":", "", [HStr, MStr, SStr]),
    number_string(Hours, HStr),
    number_string(MinutesPart, MStr),
    number_string(Seconds, SStr),
    TotalMinutes is Hours * 60 + MinutesPart + (Seconds / 60),
    Minutes is round(TotalMinutes).  % Round to the nearest minute

% Convert String to Atom
string_to_atom_safe(String, Atom) :-
    (var(String) -> Atom = 'Unknown'; string_to_atom(String, Atom)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_surgeries_data :-
        fetch_json('https://localhost:5000/api/Planning/get-surgeries', SurgeriesJSON),
        process_surgeries(SurgeriesJSON).

% Process surgeries JSON and assert them as facts
process_surgeries(SurgeriesJSON) :-
    maplist(assert_surgery, SurgeriesJSON),
    maplist(print_surgery, SurgeriesJSON).

% Assert a surgery fact
assert_surgery(Surgery) :-
    (time_to_minutes(Surgery.anesthesia, AnesthesiaMinutes) -> true ; AnesthesiaMinutes = 0),
    (time_to_minutes(Surgery.surgery, SurgeryMinutes) -> true ; SurgeryMinutes = 0),
    (time_to_minutes(Surgery.cleaning, CleaningMinutes) -> true ; CleaningMinutes = 0),
    string_to_atom_safe(Surgery.opTypeCode, OpTypeCodeAtom),
    assertz(surgery(OpTypeCodeAtom, AnesthesiaMinutes, SurgeryMinutes, CleaningMinutes)).


% Print surgery details to the console
print_surgery(Surgery) :-
    (time_to_minutes(Surgery.anesthesia, AnesthesiaMinutes) -> true ; AnesthesiaMinutes = 0),
    (time_to_minutes(Surgery.surgery, SurgeryMinutes) -> true ; SurgeryMinutes = 0),
    (time_to_minutes(Surgery.cleaning, CleaningMinutes) -> true ; CleaningMinutes = 0),
    format('Surgery ID: ~w\n', [Surgery.opTypeCode]),
    format('  Anesthesia Time: ~w minutes\n', [AnesthesiaMinutes]),
    format('  Surgery Time: ~w minutes\n', [SurgeryMinutes]),
    format('  Cleaning Time: ~w minutes\n', [CleaningMinutes]), nl.


% Clear all surgery data from the knowledge base
clear_surgeries_data :-
    retractall(surgery(_, _, _, _)),
    write('All surgery data cleared from the knowledge base.\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TODO Dinamico
% Load staff data from the backend
load_staff_data :-
    fetch_json('https://localhost:5000/api/Planning/get-staff', StaffsJSON),
    process_staffs(StaffsJSON).

% Process staff JSON and assert them as facts
process_staffs(StaffsJSON) :-
    maplist(assert_staff, StaffsJSON),
    maplist(print_staff, StaffsJSON).


% Assert a staff fact
assert_staff(Staff) :-
    % Garantir que valores nulos sejam tratados corretamente
    string_to_atom_safe(Staff.get(licenseNumber, null), LicenseNumber),
    string_to_atom_safe(Staff.get(staffType, null), StaffType),
    string_to_atom_safe(Staff.get(specialization, null), Specialization),
    SurgeryTypes = Staff.get(operationTypeCodes, []), %% VER MELHOR 
    assertz(staff(LicenseNumber, StaffType, Specialization, SurgeryTypes)).

% Print staff details to the console
print_staff(Staff) :-
    string_to_atom_safe(Staff.get(licenseNumber, null), LicenseNumber),
    string_to_atom_safe(Staff.get(staffType, null), StaffType),
    string_to_atom_safe(Staff.get(specialization, null), Specialization),
    OperationTypeCodes = Staff.get(operationTypeCodes, []),
    format('Staff License Number: ~w\n', [LicenseNumber]),
    format('  Staff Type: ~w\n', [StaffType]),
    format('  Specialization: ~w\n', [Specialization]),
    format('  Qualified Operation Types: ~w\n', [OperationTypeCodes]), nl.

% Clear all staff data from the knowledge base
clear_staff_data :-
    retractall(staff(_, _, _, _)),
    write('All staff data cleared from the knowledge base.\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%´

% Load timetable data from the backend
load_timetable_data :-
    fetch_json('https://localhost:5000/api/Planning/get-timetable', TimetablesJSON),
    process_timetables(TimetablesJSON).

% Process timetable JSON and assert them as facts
process_timetables(TimetablesJSON) :-
    maplist(assert_timetable, TimetablesJSON),
    maplist(print_timetable, TimetablesJSON).

% Clear all timetable data from the knowledge base
clear_timetable_data :-
    retractall(timetable(_, _, _)),
    write('All timetable data cleared from the knowledge base.\n').

% Assert a timetable fact with Entrance and Exit times in minutes
assert_timetable(Timetable) :-
    % Converte o licenseNumber para átomo
    string_to_atom_safe(Timetable.get(licenseNumber, null), LicenseNumber),
    
    % A data não é convertida para átomo
    DateShift = Timetable.get(date, null),  % Deixe DateShift sem conversão para átomo
    
    % Obtemos os outros campos
    TimeShiftEntrance = Timetable.get(timeShiftEntrance, null),
    TimeShiftExit = Timetable.get(timeShiftExit, null),
    
    % Converte os horários de entrada e saída para minutos
    time_to_minutes(TimeShiftEntrance, EntranceTime),
    time_to_minutes(TimeShiftExit, ExitTime),

    % Assevera o fato de timetable na base de conhecimento
    assertz(timetable(LicenseNumber, DateShift, (EntranceTime, ExitTime))).

    
% Print timetable details to the console
print_timetable(Timetable) :-
    string_to_atom_safe(Timetable.get(licenseNumber, null), LicenseNumber),
    string_to_atom_safe(Timetable.get(date, null), DateShift),
    TimeShiftEntrance = Timetable.get(timeShiftEntrance, null),
    TimeShiftExit = Timetable.get(timeShiftExit, null),
    
    % Convert the times to minutes
    time_to_minutes(TimeShiftEntrance, EntranceTime),
    time_to_minutes(TimeShiftExit, ExitTime),

    % Print the timetable details
    format('Timetable for License Number: ~w\n', [LicenseNumber]),
    format('  Date: ~w\n', [DateShift]),
    format('  Entrance Time: ~w\n', [EntranceTime]),
    format('  Exit Time: ~w\n', [ExitTime]), nl.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load surgeryId data from the backend and assert them as facts
load_surgeryId_data :-
    fetch_json('https://localhost:5000/api/Planning/get-surgeriesId', SurgeriesID),
    process_surgery_ids(SurgeriesID).

% Process surgeryId JSON and assert them as facts
process_surgery_ids(SurgeriesID) :-
    maplist(assert_surgery_id, SurgeriesID),
    maplist(print_surgery_ids, SurgeriesID). 

% Assert a surgery_id fact
assert_surgery_id(SurgeryIDData) :-
    string_to_atom_safe(SurgeryIDData.get(opRequestCode, null), OpRequestCode),
    string_to_atom_safe(SurgeryIDData.get(opTypeCode, null), OpTypeCode),
    assertz(surgery_id(OpRequestCode, OpTypeCode)).

% Clear all surgery_id data from the knowledge base
clear_surgery_id_data :-
    retractall(surgery_id(_, _)),
    write('All surgery_id data cleared from the knowledge base.\n').

% Predicado para imprimir todos os fatos surgery_id
print_surgery_ids(SurgeryIDData) :-
    string_to_atom_safe(SurgeryIDData.get(opRequestCode, null), OpRequestCode),
    string_to_atom_safe(SurgeryIDData.get(opTypeCode, null), OpTypeCode),
    format('Operation Request Code: ~w\n', [OpRequestCode]),
    format('Operation Type Code: ~w\n', [OpTypeCode]), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Clear all agenda_staff data
clear_agenda_staff_data :- 
    retractall(agenda_staff(_, _, _)),
    write('All agenda staff data cleared from the knowledge base.\n').

% Load agenda staff data from the backend
load_agenda_staff_data :- 
    fetch_json('https://localhost:5000/api/Planning/get-agenda-staff', AgendaStaffJSON),
    process_agenda_staff(AgendaStaffJSON).

% Process agenda staff JSON and assert them as facts
process_agenda_staff(AgendaStaffJSON) :- 
    maplist(assert_agenda_staff, AgendaStaffJSON),
    maplist(print_agenda_staff, AgendaStaffJSON).

% Assert a single agenda_staff fact
assert_agenda_staff(AgendaStaff) :- 
    % Extract LicenseNumber and Date from JSON and convert them to atoms
    get_dict(licenseNumber, AgendaStaff, LicenseNumberString),
    get_dict(date, AgendaStaff, DateString),
    get_dict(schedule, AgendaStaff, ScheduleStrings),
    
    % Convert LicenseNumber to atom (keep Date as string)
    string_to_atom(LicenseNumberString, LicenseNumber),
    Date = DateString,  % Keep the Date as a string
    
    % Process the schedule into a structured format
    maplist(parse_schedule_entry, ScheduleStrings, ParsedSchedule),
    
    % Assert the agenda staff fact
    assertz(agenda_staff(LicenseNumber, Date, ParsedSchedule)).

% Parse a schedule string "HH:MM:SS-HH:MM:SS-Code" into a structured term (Start, End, Code)
parse_schedule_entry(ScheduleString, (StartMinutes, EndMinutes, Code)) :- 
    % Split the schedule string into start, end, and code parts
    split_string(ScheduleString, "-", ",", [Start, End, Code]),
    
    % Convert start and end times into minutes since midnight
    time_to_minutes(Start, StartMinutes),
    time_to_minutes(End, EndMinutes).

% Print an agenda_staff entry
print_agenda_staff(AgendaStaff) :- 
    get_dict(licenseNumber, AgendaStaff, LicenseNumberString),
    get_dict(date, AgendaStaff, DateString),
    get_dict(schedule, AgendaStaff, ScheduleStrings),
    
    % Convert LicenseNumber to atom (keep Date as string)
    string_to_atom(LicenseNumberString, LicenseNumber),
    Date = DateString,  % Keep the Date as a string
    
    % Parse the schedule entries
    maplist(parse_schedule_entry, ScheduleStrings, ParsedSchedule),
    
    % Print the staff agenda
    format('Agenda Staff for License Number: ~w\n', [LicenseNumber]),
    format('  Date: ~w\n', [Date]),
    format('  Schedule: ~w\n', [ParsedSchedule]), nl.

% Print all agenda_staff facts
print_all_agenda_staff :- 
    write('Listing all agenda staff:\n'),
    forall(agenda_staff(LicenseNumber, Date, Schedule),
           format('License Number: ~w, Date: ~w, Schedule: ~w\n', [LicenseNumber, Date, Schedule])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load agenda operation room data from the backend
load_agenda_operation_room_data :- 
    fetch_json('https://localhost:5000/api/Planning/get-agenda-operation-room', AgendaOperationRoomJSON),
    process_agenda_operation_room(AgendaOperationRoomJSON).

% Process agenda operation room JSON and assert them as facts
process_agenda_operation_room(AgendaOperationRoomJSON) :-
    maplist(assert_agenda_operation_room, AgendaOperationRoomJSON),
    maplist(print_agenda_operation_room, AgendaOperationRoomJSON).

% Assert a single agenda_operation_room fact
assert_agenda_operation_room(AgendaRoom) :-
    % Extract roomNumber and date from JSON
    get_dict(roomNumber, AgendaRoom, RoomNumber),
    get_dict(date, AgendaRoom, Date),
    get_dict(schedule, AgendaRoom, ScheduleStrings),
    
    % Convert RoomNumber to atom (keep Date as string)
    string_to_atom(RoomNumber, RoomNumberAtom),
    Date = Date,  % Keep Date as a string (no conversion to atom)
    
    % Process each schedule string and convert to structured terms
    maplist(parse_schedule_entry, ScheduleStrings, ParsedSchedule),
    
    % Assert the agenda operation room fact
    assertz(agenda_operation_room(RoomNumberAtom, Date, ParsedSchedule)).

% Print an agenda_operation_room entry
print_agenda_operation_room(AgendaRoom) :-
    get_dict(roomNumber, AgendaRoom, RoomNumber),
    get_dict(date, AgendaRoom, Date),
    get_dict(schedule, AgendaRoom, ScheduleStrings),
    
    % Convert RoomNumber to atom (keep Date as string)
    string_to_atom(RoomNumber, RoomNumberAtom),
    Date = Date,  % Keep Date as a string (no conversion to atom)
    
    % Parse the schedule entries
    maplist(parse_schedule_entry, ScheduleStrings, ParsedSchedule),
    
    % Print the operation room agenda
    format('Agenda for Room: ~w, Date: ~w\n', [RoomNumberAtom, Date]),
    format('  Schedule: ~w\n', [ParsedSchedule]), nl.

% Print all agenda_operation_room facts
print_all_agenda_operation_rooms :- 
    write('Listing all agenda operation rooms:\n'),
    forall(agenda_operation_room(RoomNumber, Date, Schedule),
           format('Room Number: ~w, Date: ~w, Schedule: ~w\n', [RoomNumber, Date, Schedule])).

% Clear all agenda operation room data from the knowledge base
clear_agenda_operation_room_data :- 
    retractall(agenda_operation_room(_, _, _)),  % Remove all agenda_operation_room facts
    write('All agenda operation room data cleared from the knowledge base.\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  % Load assignment surgery data from the backend
load_assignment_surgery_data :-
    fetch_json('https://localhost:5000/api/Planning/get-assignments', AssignmentsJSON),
    process_assignment_surgeries(AssignmentsJSON).


     % Process the assignment surgery JSON and assert them as facts
process_assignment_surgeries(AssignmentsJSON) :-
    maplist(assert_assignment_surgery_from_json, AssignmentsJSON),
    maplist(print_assignment_surgery_from_json, AssignmentsJSON).

    % Assert a assignment_surgery from JSON
assert_assignment_surgery_from_json(Assignment) :-
    get_dict(opRequestCode, Assignment, OpRequestCode),
    get_dict(licenseNumber, Assignment, LicenseNumber),
    string_to_atom(OpRequestCode, OpRequestCodeAtom),
    string_to_atom(LicenseNumber, LicenseNumberAtom),
    assert_assignment_surgery(OpRequestCodeAtom, LicenseNumberAtom).
    
    % Print assignment_surgery from JSON
print_assignment_surgery_from_json(Assignment) :-
    get_dict(opRequestCode, Assignment, OpRequestCode),
    get_dict(licenseNumber, Assignment, LicenseNumber),
    string_to_atom(OpRequestCode, OpRequestCodeAtom),
    string_to_atom(LicenseNumber, LicenseNumberAtom),
    print_assignment_surgery(OpRequestCodeAtom, LicenseNumberAtom).

  % Assert a assignment_surgery fact (associando operação à equipe).
assert_assignment_surgery(OpRequestCode, LicenseNumber) :-
    string_to_atom(OpRequestCode, OpRequestCodeAtom),
    string_to_atom(LicenseNumber, LicenseNumberAtom),
    assertz(assignment_surgery(OpRequestCodeAtom, LicenseNumberAtom)).

   % Print assignment_surgery details to the console
print_assignment_surgery(OpRequestCode, LicenseNumber) :-
    format('Assignment Surgery - OpRequestCode: ~w, LicenseNumber: ~w\n', [OpRequestCode, LicenseNumber]).

  % Clear all assignment surgery data from the knowledge base
clear_assignment_surgery_data :-
    retractall(assignment_surgery(_, _)),
    write('All assignment surgery data cleared from the knowledge base.\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%