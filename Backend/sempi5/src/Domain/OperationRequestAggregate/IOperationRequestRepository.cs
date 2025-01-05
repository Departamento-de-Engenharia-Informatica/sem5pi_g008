using Sempi5.Domain.OperationRequestAggregate;
using Sempi5.Domain.Shared;

namespace Sempi5.Infrastructure.OperationRequestAggregate;

public interface IOperationRequestRepository : IRepository<OperationRequest, OperationRequestID>
{
    Task RemoveAsync(OperationRequest appointmentOperationRequest);
    Task<List<OperationRequest>> SearchAsync(string patientName, string type, string priority);
    Task<OperationRequest?> GetByDoctorId(string doctorId);
    Task UpdateAsync(OperationRequest request);
    public Task<OperationRequest> GetOperationRequestById(string id);

    public Task<List<OperationRequest>> GetDoctorsOperationsRequests(string doctorId);

    Task<List<OperationRequest>> SearchAsyncWithDoctocId(string patientName, string type, string priority, string doctorId);
   Task<List<OperationRequest>> GetallOperationRequest();
}