using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Domain.StaffAggregate;

namespace Sempi5.Infrastructure.StaffAggregate
{
    public interface IStaffRepository : IRepository<Staff, StaffId>
    {
        public Task<Staff> GetByEmail(string email);
        
        public Task<Staff?> GetActiveStaffById(StaffId id);
        
        public Task<Staff?> GetByLicenseNumber(LicenseNumber licenseNumber);
        
        public Task<List<Staff>> GetActiveStaffByName(Name fullName);

        public Task<Staff> GetActiveStaffByEmail(Email email);
        
        public Task<List<Staff>> GetActiveStaffBySpecialization(SpecializationName specializationName);
        
        public Task<List<Staff>> GetAllActiveStaff();
        public Task update(Staff staffUpdate);
    }
}