using Sempi5.Domain;
using Sempi5.Domain.AgendaAggregate;

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
    public async Task<List<RoomAgenda>>getAllRoomAgenda()
    {
       return await _roomAgendaRepository.GetAllRoomAgenda();
    }
    public async Task<List<StaffAgenda>>getAllStaffAgenda()
    {
         return await _staffAgendaRepository.GetAllStaffAgenda();
    }
}