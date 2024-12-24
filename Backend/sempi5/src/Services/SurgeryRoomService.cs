using Sempi5.Domain.Shared;
using Sempi5.Domain.SurgeryRoomAggregate;
using Sempi5.Infrastructure.SurgeryRoomAggregate;

namespace Sempi5.Services;

public class SurgeryRoomService
{
 private readonly ISurgeryRoomRepository _surgeryRoomRepository;
 private readonly IUnitOfWork _unitOfWork;
 
 public SurgeryRoomService(ISurgeryRoomRepository surgeryRoomRepository, IUnitOfWork unitOfWork)
 {
     _surgeryRoomRepository = surgeryRoomRepository;
     _unitOfWork = unitOfWork;
 }
 
 public async Task<List<bool>> getAllSurgeryRoomsOccupancy()
 {
     var surgeryRooms = await _surgeryRoomRepository.GetAllAsync();
     var occupancy = new List<bool>();
     foreach (var room in surgeryRooms)
     {
         occupancy.Add(room.Status == RoomStatusEnum.OCCUPIED);
     }
     return occupancy;
 }
 
 public async Task<List<List<Dictionary<string, string>>>> getRoomsInfo()
 {
     var dummyData = new List<List<Dictionary<string, string>>>();

     for (var i = 1; i <= 6; i++)
     {
         var roomData = new List<Dictionary<string, string>>
         {
             
             new Dictionary<string, string> { { "title", $"Parameter  CHANGE" }, { "body", $"Value  BACKEND" } },
             new Dictionary<string, string> { { "title", $"Parameter  NOT" }, { "body", $"Value  IMPLEMENTED" } }
         };

         dummyData.Add(roomData);
     }

     return await Task.FromResult(dummyData);
 }


}