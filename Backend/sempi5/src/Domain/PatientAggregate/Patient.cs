using Sempi5.Domain.PersonalData;
using Sempi5.Domain.Shared;
using Sempi5.Domain.User;

namespace Sempi5.Domain.PatientAggregate
{
    public class Patient : Entity<MedicalRecordNumber>, IAggregateRoot
    {
        
        public SystemUser? User { get; set; }      
        public MedicalRecordNumber Id { get; set; }
        public Person? Person { get; set; }
        public DateTime BirthDate { get; set; }
        public string Gender { get; set; }
        public List<string>? AllergiesAndMedicalConditions { get; set; }
        public string? EmergencyContact { get; set; }
        public List<string>? AppointmentHistory { get; set; }
        public PatientStatusEnum PatientStatus { get; set; }

        private Patient() {}
        
        public Patient(SystemUser user, Person person, DateTime birthDate, string gender, List<string> allergiesAndMedicalConditions, string emergencyContact, List<string> appointmentHistory)
        {
            ArgumentNullException.ThrowIfNull(user);
            ArgumentNullException.ThrowIfNull(person);
            ArgumentException.ThrowIfNullOrWhiteSpace(gender);
            ArgumentException.ThrowIfNullOrWhiteSpace(emergencyContact);
            ArgumentNullException.ThrowIfNull(allergiesAndMedicalConditions);
            ArgumentNullException.ThrowIfNull(appointmentHistory);
            
            User = user;
            Person = person;
            BirthDate = birthDate;
            Gender = gender;
            AllergiesAndMedicalConditions = allergiesAndMedicalConditions;
            EmergencyContact = emergencyContact;
            AppointmentHistory = appointmentHistory;
            PatientStatus = PatientStatusEnum.ACTIVATED;
        }
        
        public Patient(SystemUser user, Person person, DateTime birthDate, string gender, List<string> allergiesAndMedicalConditions, string emergencyContact, List<string> appointmentHistory, PatientStatusEnum patientStatus)
        {
            ArgumentNullException.ThrowIfNull(user);
            ArgumentNullException.ThrowIfNull(person);
            ArgumentException.ThrowIfNullOrWhiteSpace(gender);
            ArgumentException.ThrowIfNullOrWhiteSpace(emergencyContact);
            ArgumentNullException.ThrowIfNull(allergiesAndMedicalConditions);
            ArgumentNullException.ThrowIfNull(appointmentHistory);
            
            User = user;
            Person = person;
            BirthDate = birthDate;
            Gender = gender;
            AllergiesAndMedicalConditions = allergiesAndMedicalConditions;
            EmergencyContact = emergencyContact;
            AppointmentHistory = appointmentHistory;
            PatientStatus = patientStatus;
        }
        
        public Patient( Person person, DateTime birthDate, string gender, string emergencyContact)
        {
            ArgumentNullException.ThrowIfNull(person);
            ArgumentException.ThrowIfNullOrWhiteSpace(gender);
            ArgumentException.ThrowIfNullOrWhiteSpace(emergencyContact);
            
            Person = person;
            BirthDate = birthDate;
            Gender = gender;
            AllergiesAndMedicalConditions = new List<string>();
            EmergencyContact = emergencyContact;
            AppointmentHistory =  new List<string>();
            PatientStatus = PatientStatusEnum.DEACTIVATED;
        }
        public Patient(Person person, DateTime birthDate, string gender, List<string> allergiesAndMedicalConditions, string emergencyContact, List<string> appointmentHistory, PatientStatusEnum patientStatus)
        {
            ArgumentNullException.ThrowIfNull(person);
            ArgumentException.ThrowIfNullOrWhiteSpace(gender);
            ArgumentException.ThrowIfNullOrWhiteSpace(emergencyContact);
            ArgumentNullException.ThrowIfNull(allergiesAndMedicalConditions);
            ArgumentNullException.ThrowIfNull(appointmentHistory);
            
            User = null;
            Person = person;
            BirthDate = birthDate;
            Gender = gender;
            AllergiesAndMedicalConditions = allergiesAndMedicalConditions;
            EmergencyContact = emergencyContact;
            AppointmentHistory = appointmentHistory;
            PatientStatus = patientStatus;
        }
        
        
        
    }
}