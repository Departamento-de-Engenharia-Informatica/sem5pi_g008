using Sempi5.Domain.PersonalData;
using Sempi5.Domain.Shared;
using Sempi5.Domain.User;
using Sempi5.Domain.SpecializationAggregate;

namespace Sempi5.Domain.StaffAggregate
{
    public class Staff : Entity<StaffId>, IAggregateRoot
    {

        public SystemUser? User { get; set; }
        public StaffId Id { get; set; }
        public Person? Person { get; set; }
        public LicenseNumber? LicenseNumber { get; set; }
        public Specialization Specialization { get; set; }
        public List<string> AvailabilitySlots { get; set; }
        public StaffStatusEnum Status { get; set; }
        public List<StaffAgenda> StaffAgendas { get; set; }
        
        private Staff() { }
        
        public Staff(LicenseNumber licenseNumber, Person person, Specialization specialization)
        {
            
            ArgumentNullException.ThrowIfNull(licenseNumber);
            ArgumentNullException.ThrowIfNull(person);
            ArgumentNullException.ThrowIfNull(specialization);
            
            User = null;
            LicenseNumber = licenseNumber;
            Specialization = specialization;
            AvailabilitySlots = new List<string>();
            Person = person;
            Status = StaffStatusEnum.INACTIVE;
        }
        
        public Staff(SystemUser user,LicenseNumber licenseNumber, Person person, Specialization specialization, StaffStatusEnum status)
        {
            
            ArgumentNullException.ThrowIfNull(licenseNumber);
            ArgumentNullException.ThrowIfNull(person);
            ArgumentNullException.ThrowIfNull(specialization);
            
            User = user;
            LicenseNumber = licenseNumber;
            Specialization = specialization;
            AvailabilitySlots = new List<string>();
            Person = person;
            Status = status;
        }
        
        public Staff(SystemUser user, LicenseNumber licenseNumber, Person person, Specialization specialization,
            StaffStatusEnum status, List<StaffAgenda> staffAgendas)
        {
            
            ArgumentNullException.ThrowIfNull(licenseNumber);
            ArgumentNullException.ThrowIfNull(person);
            ArgumentNullException.ThrowIfNull(specialization);
            
            User = user;
            LicenseNumber = licenseNumber;
            Specialization = specialization;
            AvailabilitySlots = new List<string>();
            Person = person;
            Status = status;
            StaffAgendas = staffAgendas;
        }
        public Staff(SystemUser user, LicenseNumber licenseNumber, Person person, Specialization specialization,
            StaffStatusEnum status, List<StaffAgenda> staffAgendas, List<string> list)
        {
            
            ArgumentNullException.ThrowIfNull(licenseNumber);
            ArgumentNullException.ThrowIfNull(person);
            ArgumentNullException.ThrowIfNull(specialization);
            
            User = user;
            LicenseNumber = licenseNumber;
            Specialization = specialization;
            AvailabilitySlots = list;
            Person = person;
            Status = status;
            StaffAgendas = staffAgendas;
        }
        
        
        public void AddUser(SystemUser user)
        {
            User = user;
        }
        
        public void markAsActive()
        {
            Status = StaffStatusEnum.ACTIVE;
        }
        
    }
}