using Sempi5.Domain.PersonalData;
using Sempi5.Domain.Shared;

namespace Sempi5.Domain.AgendaAggregate;

public interface IRoomAgendaRepository: IRepository<RoomAgenda, AgendaId>
{
    public Task<List<RoomAgenda>> GetAllRoomAgenda();
    public Task<List<RoomAgenda>> GetRoomAgendasByRoomIdAndDay(AgendaId roomId, DateTime day);
}