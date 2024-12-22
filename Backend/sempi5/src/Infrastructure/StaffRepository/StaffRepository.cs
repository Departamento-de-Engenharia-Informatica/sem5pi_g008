using Microsoft.EntityFrameworkCore;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Domain.StaffAggregate;
using Sempi5.Infrastructure.Databases;
using Sempi5.Infrastructure.Shared;
using Sempi5.Infrastructure.StaffAggregate;

namespace Sempi5.Infrastructure.StaffRepository
{
    public class StaffRepository : BaseRepository<Staff,StaffId>, IStaffRepository
    {

        private readonly DBContext context;

        public StaffRepository(DBContext context) : base(context.StaffMembers)
        { this.context = context; }

        public async Task<Staff> GetByEmail(string email)
        {
            if (string.IsNullOrEmpty(email))
            {
                return null;
            }

            Email emailObj = new Email(email);
            
            var staff = await Task.Run(() => context.StaffMembers
                .Include(u => u.User)
                .Include(u => u.Person)
                .FirstOrDefault(u => u.User.Email.Equals(emailObj)));

            return staff;
        }

        
        public Task<Staff?> GetActiveStaffById(StaffId id)
        {
            if (id == null)
            {
                return null;
            }

            return context.StaffMembers.Include(p => p.Person)
                .Include(p => p.Specialization)
                .Include(p=>p.User)
                .Include(p => p.Person)
                .FirstOrDefaultAsync(p => p.Id.Equals(id) && p.Status.Equals(StaffStatusEnum.ACTIVE)); //&& p.Status.Equals(StaffStatusEnum.ACTIVE));
        }
        
        public Task<Staff?> GetByLicenseNumber(LicenseNumber licenseNumber)
        {
            if (licenseNumber == null)
            {
                return null;
            }
            
            return context.StaffMembers.FirstOrDefaultAsync(p => p.LicenseNumber.Equals(licenseNumber));
        }

        public async Task<List<Staff>> GetActiveStaffByName(Name fullName)
        {
            if (fullName == null)
            {
                return null;
            }
            
            return await context.StaffMembers
                .Include(p => p.Person)
                .Include(p => p.Specialization)
                .Where(p => p.Person.FullName.Equals(fullName)
                && p.Status.Equals(StaffStatusEnum.ACTIVE))
                .ToListAsync();
        }

        public async Task<List<Staff>> GetActiveStaffBySpecialization(SpecializationName specializationName)
        {
            if (specializationName == null)
            {
                return null;
            }
            
            return await context.StaffMembers
                .Include(p => p.Person)
                .Include(p => p.Specialization)
                .Where(p => p.Specialization.specializationName.Equals(specializationName)
                && p.Status.Equals(StaffStatusEnum.ACTIVE))
                .ToListAsync();
        }
        
        public async Task<List<Staff>> GetAllActiveStaff()
        {
            return await context.StaffMembers
                .Include(p => p.Person)
                .Include(p => p.Specialization)
                .Where(p => p.Status.Equals(StaffStatusEnum.ACTIVE))
                .ToListAsync();
        }

        public Task<Staff?> GetActiveStaffByEmail(Email email)
        {
            
            if (email == null)
            {
                return null;
            }
            
            return context.StaffMembers
                .Include(p => p.Person)
                .Include(p => p.Specialization)
                .FirstOrDefaultAsync(p => p.Person.ContactInfo._email.Equals(email) && p.Status.Equals(StaffStatusEnum.ACTIVE));
        }

        public async Task<Staff> GetByIdAsync(StaffId id)
        {
            //return await this._context.Categories.FindAsync(id);
            var staff = await Task.Run(() => context.StaffMembers
                .Include(x => x.User)
                .Include(x => x.Person)
                .Where(x => id.Equals(x.Id)).FirstOrDefaultAsync()
                );
            
            return staff;
        }
        
    }
}
