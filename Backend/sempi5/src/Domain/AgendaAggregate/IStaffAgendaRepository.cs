using Sempi5.Domain.PersonalData;
using Sempi5.Domain.Shared;

namespace Sempi5.Domain.AgendaAggregate;

public interface IStaffAgendaRepository : IRepository<StaffAgenda, AgendaId>
{
    public Task<List<StaffAgenda>> GetAllStaffAgenda();
}