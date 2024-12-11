using Sempi5.Domain.AgendaAggregate;
using Sempi5.Domain.PersonalData;
using Sempi5.Domain.Shared;

namespace Sempi5.Domain;

public class RoomAgenda: Entity<AgendaId>, IAggregateRoot
{
    public DateTime Date { get;  set; }
    public List<String> TimeIntervals { get;  set; }
}