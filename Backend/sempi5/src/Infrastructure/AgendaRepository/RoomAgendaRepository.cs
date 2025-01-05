using Microsoft.EntityFrameworkCore;
using Sempi5.Domain;
using Sempi5.Domain.AgendaAggregate;
using Sempi5.Domain.PersonalData;
using Sempi5.Infrastructure.Databases;
using Sempi5.Infrastructure.Shared;

namespace Sempi5.Infrastructure;

public class RoomAgendaRepository:BaseRepository<RoomAgenda, AgendaId>, IRoomAgendaRepository
{
    private readonly DBContext context;
    public RoomAgendaRepository(DBContext context) : base(context.RoomAgendas)
    {
        this.context = context;
    }

    public async Task<List<RoomAgenda>> GetAllRoomAgenda()
    {
        return await context.RoomAgendas.ToListAsync();
    }

    public Task updateAgenda()
    {
        throw new NotImplementedException();
    }
    
    public async Task<List<RoomAgenda>> GetRoomAgendasByRoomIdAndDay(AgendaId roomId, DateTime day)
    {
        return await context.RoomAgendas
            .Where(ra => ra.Id.Equals(roomId) && ra.Date.Date.Equals(day.Date))
            .ToListAsync();
    }
}