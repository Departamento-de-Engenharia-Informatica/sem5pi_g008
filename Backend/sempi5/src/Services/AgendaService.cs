using System.Globalization;
using Sempi5.Domain;
using Sempi5.Domain.AgendaAggregate;
using Sempi5.Domain.AppointmentAggregate;
using Sempi5.Domain.OperationRequestAggregate;
using Sempi5.Domain.PatientAggregate;
using Sempi5.Domain.StaffAggregate;
using Sempi5.Domain.SurgeryRoomAggregate;
using Sempi5.Infrastructure.StaffAggregate;
using Sempi5.Infrastructure.SurgeryRoomAggregate;

namespace Sempi5.Services;

public class AgendaService
{
    private readonly IRoomAgendaRepository _roomAgendaRepository;
    private readonly IStaffAgendaRepository _staffAgendaRepository;
    private readonly IStaffRepository _staffRepository;
    private readonly ISurgeryRoomRepository _surgeryRoomRepository;



    public AgendaService(IRoomAgendaRepository roomAgendaRepository, IStaffAgendaRepository staffAgendaRepository, IStaffRepository staffRepository, ISurgeryRoomRepository surgeryRoomRepository)
    {
        _roomAgendaRepository = roomAgendaRepository;
        _staffAgendaRepository = staffAgendaRepository;
        _staffRepository = staffRepository;
        _surgeryRoomRepository = surgeryRoomRepository;
    }

    //TODO:see how to get staffid and room id
    public async Task<List<RoomAgenda>> getAllRoomAgenda()
    {
        return await _roomAgendaRepository.GetAllRoomAgenda();
    }
    
    public async Task<List<StaffAgenda>> getAllStaffAgenda()
    {
        return await _staffAgendaRepository.GetAllStaffAgenda();
    }
    
    // public async Task createAppointment(AppointmentHistoryDTO appointment)
    // {
    //     var operationRequest = new OperationRequest();
    //     var surgeryRoom = new SurgeryRoom();
    //     var appointment1 = new Appointment(operationRequest,
    //     surgeryRoom, new DateTime(2021, 1, 1),
    //     StatusEnum.SCHEDULED);
    // }
    
    public async Task updateAgendaRoom(AgendaDto appointment)
    {
        var date = DateTime.ParseExact(appointment.date.ToString(), "yyyyMMdd", CultureInfo.InvariantCulture);
        var timeIntervals = new List<string>();
        var appointmentType="";
        for (int i = 0; i < appointment.task.Count; i++)
        {
            // Obtendo o tipo de compromisso
             appointmentType = appointment.task[i].code;

            // Convertendo minutos para o formato de hora HH:mm
            var startTime = TimeSpan.FromMinutes(appointment.task[i].start).ToString(@"hh\:mm");
            var endTime = TimeSpan.FromMinutes(appointment.task[i].end).ToString(@"hh\:mm");

            // Montando o intervalo de tempo
            var timeInterval = $"{startTime}-{endTime}";
            Console.WriteLine(timeInterval);

            // Adicionando o intervalo à lista de intervalos de tempo
            timeIntervals.Add(timeInterval);
        }
        var roomAgenda = new RoomAgenda
        {
            Date = date,
            TimeIntervals = timeIntervals,
            appointmentType = appointmentType
        };
        var surgeryRoomUpdate = await _surgeryRoomRepository.GetSurgeryRoomById(int.Parse(appointment.room_id));
        surgeryRoomUpdate.RoomAgendas.Add(roomAgenda);
        await _surgeryRoomRepository.updateRoomAgenda(surgeryRoomUpdate);
    }
    public async Task updateAgendaStaff(AgendaDto appointment)
    {
        var staffUpdate= await _staffRepository.GetActiveStaffById(new StaffId(appointment.room_id));
        var date = DateTime.ParseExact(appointment.date.ToString(), "yyyyMMdd", CultureInfo.InvariantCulture);
        var timeIntervals = new List<string>();
        var appointmentType="";
        for (int i = 0; i < appointment.task.Count; i++)
        { 
            appointmentType = appointment.task[i].code;

            // Convertendo minutos para o formato de hora HH:mm
            var startTime = TimeSpan.FromMinutes(appointment.task[i].start).ToString(@"hh\:mm");
            var endTime = TimeSpan.FromMinutes(appointment.task[i].end).ToString(@"hh\:mm");

            // Montando o intervalo de tempo
            var timeInterval = $"{startTime}-{endTime}";
            Console.WriteLine(timeInterval);

            // Adicionando o intervalo à lista de intervalos de tempo
            timeIntervals.Add(timeInterval);
        }
        var staffAgenda = new StaffAgenda()
        {
            Date = date,
            TimeIntervals = timeIntervals,
            appointmentType = appointmentType
        };       
        
        staffUpdate.StaffAgendas.Add(staffAgenda);
        await _staffRepository.update(staffUpdate);
    }
}