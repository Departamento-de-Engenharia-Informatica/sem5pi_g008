
using Microsoft.EntityFrameworkCore;
using Sempi5.Domain.SurgeryRoomAggregate;
using Sempi5.Infrastructure.Databases;
using Sempi5.Infrastructure.Shared;
using Sempi5.Infrastructure.SurgeryRoomAggregate;

namespace Sempi5.Infrastructure.SurgeryRoomRepository
{
    public class SurgeryRoomRepository : BaseRepository<SurgeryRoom, RoomNumber>, ISurgeryRoomRepository
    {
        DBContext context;
        public SurgeryRoomRepository(DBContext context):base(context.SurgeryRooms)
        { this.context = context; }



        public async Task<List<SurgeryRoom>> GetAllStaff()
        {
            return await context.SurgeryRooms
                .Include(r=>r.RoomAgendas)
                .ToListAsync();

        }
   public async Task<SurgeryRoom> GetSurgeryRoomById(int id)
{
    var surgeryRoom = await context.SurgeryRooms
        .FirstOrDefaultAsync(r => r.Id.Equals(new RoomNumber(id)));
        
    return surgeryRoom;
}
    }
}