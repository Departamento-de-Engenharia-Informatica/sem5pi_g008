using Microsoft.EntityFrameworkCore;
using Sempi5.Domain;
using Sempi5.Domain.AgendaAggregate;
using Sempi5.Domain.PersonalData;
using Sempi5.Infrastructure.Databases;
using Sempi5.Infrastructure.Shared;

namespace Sempi5.Infrastructure;

public class StaffAgendaRepository: BaseRepository<StaffAgenda, AgendaId>, IStaffAgendaRepository
{
    private readonly DBContext context;
    public StaffAgendaRepository(DBContext context) : base(context.StaffAgendas)
    {
        this.context = context;
    }

    public async Task<List<StaffAgenda>> GetAllStaffAgenda()
    {
        return await context.StaffAgendas.ToListAsync();
    }

}

