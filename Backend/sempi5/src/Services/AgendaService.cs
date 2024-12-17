using Sempi5.Domain;
using Sempi5.Domain.AgendaAggregate;
using Sempi5.Domain.AppointmentAggregate;
using Sempi5.Domain.PatientAggregate;

namespace Sempi5.Services;

public class AgendaService
{
    private readonly IRoomAgendaRepository _roomAgendaRepository;
    private readonly IStaffAgendaRepository _staffAgendaRepository;

    public AgendaService(IRoomAgendaRepository roomAgendaRepository, IStaffAgendaRepository staffAgendaRepository)
    {
        _roomAgendaRepository = roomAgendaRepository;
        _staffAgendaRepository = staffAgendaRepository;
    }

    //TODO:see how to get staffid and room id
    // public async Task<List<RoomAgenda>> getAllRoomAgenda()
    // {
    //     return await _roomAgendaRepository.GetAllRoomAgenda();
    // }
    //
    // public async Task<List<StaffAgenda>> getAllStaffAgenda()
    // {
    //     return await _staffAgendaRepository.GetAllStaffAgenda();
    // }
    //
    // public async Task createAppointment(AppointmentHistoryDTO appointment)
    // {
    //     // var operationRequest = new OperationRequest();
    //     // var surgeryRoom = new SurgeryRoom();
    //     // var appointment1 = new Appointment(operationRequest,
    //     // surgeryRoom, new DateTime(2021, 1, 1),
    //     // StatusEnum.SCHEDULED);
    // }
    //
    // public async Task updateAgendaRoom(AgendaDto appointment)
    // {
    //     // var roomAgenda = new RoomAgenda();
    //     // var staffAgenda = new StaffAgenda();
    //     // roomAgenda.addAppointment(appointment);
    //     // staffAgenda.addAppointment(appointment);
    //     await _roomAgendaRepository.updateAgenda();
    //
    // }
    // public async Task updateAgendaStaff(AgendaDto appointment)
    // {
    //     // var roomAgenda = new RoomAgenda();
    //     // var staffAgenda = new StaffAgenda();
    //     // roomAgenda.addAppointment(appointment);
    //     // staffAgenda.addAppointment(appointment);
    //      await _staffAgendaRepository.updateAgenda();
    //
    // }
}