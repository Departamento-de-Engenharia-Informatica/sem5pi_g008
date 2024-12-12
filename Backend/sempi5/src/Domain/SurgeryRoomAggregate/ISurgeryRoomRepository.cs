
using Sempi5;
using Sempi5.Domain;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SurgeryRoomAggregate;

namespace Sempi5.Infrastructure.SurgeryRoomAggregate
{
    public interface ISurgeryRoomRepository: IRepository<SurgeryRoom, RoomNumber>
    {
        Task<List<SurgeryRoom>> GetAllStaff();
    }
}