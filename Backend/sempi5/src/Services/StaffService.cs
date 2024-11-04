using System.Text.Json;
using Microsoft.CodeAnalysis.Elfie.Serialization;
using Sempi5.Domain.AppointmentAggregate;
using Sempi5.Domain.Encrypt;
using Sempi5.Domain.OperationRequestAggregate;
using Sempi5.Domain.PatientAggregate;
using Sempi5.Domain.PersonalData;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Domain.StaffAggregate;
using Sempi5.Domain.StaffAggregate.DTOs;
using Sempi5.Infrastructure.AppointmentAggregate;
using Sempi5.Infrastructure.AppointmentRepository;
using Sempi5.Infrastructure.OperationRequestAggregate;
using Sempi5.Infrastructure.OperationRequestRepository;
using Sempi5.Infrastructure.PatientAggregate;
using Sempi5.Infrastructure.PatientRepository;
using Sempi5.Infrastructure.PersonAggregate;
using Sempi5.Infrastructure.PersonRepository;
using Sempi5.Infrastructure.SpecializationAggregate;
using Sempi5.Infrastructure.SpecializationRepository;
using Sempi5.Infrastructure.StaffAggregate;
using Sempi5.Infrastructure.StaffRepository;
using Sempi5.Infrastructure.UserRepository;

namespace Sempi5.Services
{
    public class StaffService
    {
        private readonly IStaffRepository _staffRepository;
        private readonly ISpecializationRepository _specializationRepository;
        private readonly IPersonRepository _personRepository;
        private readonly IUnitOfWork _unitOfWork;
        private readonly IAppointmentRepository _appointmentRepository;
        private readonly IOperationRequestRepository _operationRequestRepository;

        public StaffService(IStaffRepository staffRepository,
            ISpecializationRepository specializationRepository, IPersonRepository personRepository,
            IUnitOfWork unitOfWork, IAppointmentRepository appointmentRepository,
            IOperationRequestRepository operationRequestRepository)
        {
            _staffRepository = staffRepository;
            _specializationRepository = specializationRepository;
            _unitOfWork = unitOfWork;
            _specializationRepository = specializationRepository;
            _personRepository = personRepository;
            _appointmentRepository = appointmentRepository;
            _operationRequestRepository = operationRequestRepository;
        }


        public async Task CreateStaffProfile(StaffDTO staffDTO)
        {
            var staff = await StaffDtoToStaff(staffDTO);

            await _staffRepository.AddAsync(staff);

            await _unitOfWork.CommitAsync();
        }

        private async Task<Staff> StaffDtoToStaff(StaffDTO staffDTO)
        {
            var licenseNumber = new LicenseNumber(staffDTO.LicenseNumber);

            await VerifyLicenseNumberAvailability(licenseNumber);

            var person = await CreatePerson(staffDTO.FirstName, staffDTO.LastName, staffDTO.Email,
                staffDTO.PhoneNumber);

            var specialization = await CreateSpecialization(staffDTO.Specialization);

            return new Staff(licenseNumber, person, specialization);
        }

        private async Task VerifyLicenseNumberAvailability(LicenseNumber licenseNumber)
        {
            var staffByLicenseNumber = await _staffRepository.GetByLicenseNumber(licenseNumber);

            if (staffByLicenseNumber != null)
            {
                throw new ArgumentException("License Number already in use.");
            }
        }


        private async Task<Specialization> CreateSpecialization(string specializationName)
        {
            var specialiName = new SpecializationName(specializationName);

            var specialization = new Specialization(specialiName);

            var searchedSpecialization = await _specializationRepository.GetBySpecializationName(specialization);

            if (searchedSpecialization != null)
            {
                return searchedSpecialization;
            }

            return specialization;
        }

        private async Task<Person> CreatePerson(string firstName, string lastName, string emailString,
            int phoneNumberInt)
        {
            
            await VerifyPhoneNumberAvailability(phoneNumberInt);
            
            var phoneNumber = new PhoneNumber(phoneNumberInt);
            
            await VerifyEmailAvailability(emailString);

            var email = new Email(emailString);
            
            var contactInfo = new ContactInfo(email, phoneNumber);
            var person = new Person(new Name(firstName), new Name(lastName), contactInfo);

            return person;
        }

        public async Task VerifyPhoneNumberAvailability(int phoneNumber)
        {
            var phone = new PhoneNumber(phoneNumber);
            
            var personByPhoneNumber = await _personRepository.GetPersonByPhoneNumber(phone);

            if (personByPhoneNumber != null)
            {
                throw new ArgumentException("Phone Number already in use.");
            }
        }

        public async Task VerifyEmailAvailability(string email)
        {

            var emailAdd = new Email(email);
            
            var personByEmail = await _personRepository.GetPersonByEmail(emailAdd);

            if (personByEmail != null)
            {
                throw new ArgumentException("Email already in use.");
            }
        }

        public async Task<Staff> GetStaffById(string id)
        {
            var staff = await _staffRepository.GetActiveStaffById(new StaffId(id));

            if (staff == null)
            {
                throw new ArgumentException("Staff not found.");
            }

            return staff;
        }


        public async Task<StaffDTO> EditStaffProfile(EditStaffDTO editStaffDto)
        {
            var staff = await _staffRepository.GetActiveStaffById(new StaffId(editStaffDto.Id));

            if (staff == null)
            {
                throw new ArgumentException("Staff not found.");
            }

            if (editStaffDto.email != null)
            {
                await VerifyEmailAvailability(editStaffDto.email);

                var email = new Email(editStaffDto.email);
                
                staff.Person.ContactInfo._email = email;
            }


            if (editStaffDto.phoneNumber > 0)
            {

                await VerifyPhoneNumberAvailability(editStaffDto.phoneNumber);

                var phoneNumber = new PhoneNumber(editStaffDto.phoneNumber);
                
                staff.Person.ContactInfo._phoneNumber = phoneNumber;
            }

            if (editStaffDto.specialization != null)
            {
                staff.Specialization = await CreateSpecialization(editStaffDto.specialization);
            }

            await _unitOfWork.CommitAsync();

            return await StaffToStaffDto(staff);
        }

        public async Task<StaffDTO> StaffToStaffDto(Staff staff)
        {
            return new StaffDTO
            {
                FirstName = staff.Person.FirstName.ToString(),
                LastName = staff.Person.LastName.ToString(),
                LicenseNumber = staff.LicenseNumber.licenseNumber(),
                Email = staff.Person.ContactInfo._email.ToString(),
                PhoneNumber = staff.Person.ContactInfo._phoneNumber.phoneNumber(),
                Specialization = staff.Specialization.specializationName.ToString()
            };
        }

        public async Task DeactivateStaffProfile(StaffIdDTO staffId)
        {
            var staff = await _staffRepository.GetActiveStaffById(new StaffId(staffId.Id));

            if (staff == null)
            {
                throw new ArgumentException("Staff not found.");
            }

            staff.Status = StaffStatusEnum.INACTIVE;
            staff.Person = null;
            staff.LicenseNumber = null;
            
            await _unitOfWork.CommitAsync();
        }

        public async Task<List<SearchedStaffDTO>> ListStaffByName(NameDTO nameDto)
        {
            var staffList = await _staffRepository.GetActiveStaffByName(new Name(nameDto.name));

            if (staffList.Count == 0)
            {
                throw new ArgumentException("Staffs not found.");
            }

            var staffDtoList = BuildStaffDtoList(staffList);

            return staffDtoList;
        }

        public async Task<SearchedStaffDTO> ListStaffByEmail(EmailDTO emailDto)
        {
            var staff = await _staffRepository.GetActiveStaffByEmail(new Email(emailDto.email));

            if (staff == null)
            {
                throw new ArgumentException("Staff not found.");
            }

            return StaffToSearchedStaffDto(staff);
        }

        public async Task<List<SearchedStaffDTO>> ListStaffBySpecialization(SpecializationNameDTO specializationDto)
        {
            
            var specializationName = new SpecializationName(specializationDto.specializationName);
            var specializationToSearch = new Specialization(specializationName);
            
            var specialization = await 
                _specializationRepository.GetBySpecializationName(specializationToSearch);
            
            if (specialization == null)
            {
                throw new ArgumentException("Specialization not found.");
            }
            
            var staffList =
                await _staffRepository.GetActiveStaffBySpecialization(
                    new SpecializationName(specializationDto.specializationName));

            if (staffList.Count == 0)
            {
                throw new ArgumentException("Staffs not found.");
            }

            var staffDtoList = BuildStaffDtoList(staffList);

            return staffDtoList;
        }
        
        public async Task<List<SearchedStaffDTO>> ListAllStaff()
        {
            var staffList = await _staffRepository.GetAllActiveStaff();

            if (staffList.Count == 0)
            {
                throw new ArgumentException("Staffs not found.");
            }

            var staffDtoList = BuildStaffDtoList(staffList);

            return staffDtoList;
        }

        private SearchedStaffDTO StaffToSearchedStaffDto(Staff staff)
        {
            return new SearchedStaffDTO
            {
                Id = staff.Id.AsString(),
                FullName = staff.Person.FullName.ToString(),
                Email = staff.Person.ContactInfo.email().ToString(),
                Specialization = staff.Specialization.specializationName.ToString()
            };
        }

        public async Task<bool> DeleteRequestAsync(string doctorEmail)
        {
            var staff = await _staffRepository.GetByEmail(doctorEmail);
            var doctorId = staff.Id.AsString();
            var operationRequest = await _operationRequestRepository.GetByDoctorId(doctorId);
            if (operationRequest == null)
            {
                throw new InvalidOperationException("No operation request found for this doctor.");
            }


            if (operationRequest != null)
            {
                var appointment =
                    await _appointmentRepository.getAppointmentByOperationRequestID(operationRequest.Id.AsLong());
                if (appointment == null)
                    throw new UnauthorizedAccessException("No operation request found for this doctor.");

                if (appointment.Status.Equals(StatusEnum.SCHEDULED) ||
                    appointment.Status.Equals(StatusEnum.COMNPLETED) ||
                    appointment.Status.Equals(StatusEnum.CANCELED))
                    throw new InvalidOperationException("Cannot delete a scheduled operation.");

                appointment.Status = StatusEnum.CANCELED;

                await _appointmentRepository.updataAppointment(appointment);
                await _operationRequestRepository.RemoveAsync(appointment.OperationRequest);
                await _appointmentRepository.SaveChangesAsync();
            }

            //TODO: create A NOTIFICATION SERVICE
            //await _planningService.NotifyOperationDeleted(appointmentId);
            return true;
        }

        private List<SearchedStaffDTO> BuildStaffDtoList(List<Staff> staffs)
        {
            List<SearchedStaffDTO> searchedStaffDtoList = new List<SearchedStaffDTO>();

            foreach (var staff in staffs)
            {
                searchedStaffDtoList.Add(StaffToSearchedStaffDto(staff));
            }

            return searchedStaffDtoList;
        }


       public async Task<List<OperationRequest>> SearchRequestsAsync(string patientName, string type, string priority, string status)
{
    try
    {
        List<OperationRequest> operationRequests = new List<OperationRequest>();
        List<OperationRequest> operationRequests_status = new List<OperationRequest>();
        operationRequests = await _operationRequestRepository.SearchAsync(patientName, type, priority);
        if (status != null)
        {
            for (int i = 0; i < operationRequests.Count; i++)
            {
                var operationRequest = operationRequests[i];
                var appointment = await _appointmentRepository.getAppointmentByOperationRequestID(operationRequest.Id.AsLong());
                if (appointment.Status.ToString().ToLower().Equals(status.ToLower()))
                {
                    operationRequests_status.Add(appointment.OperationRequest);
                }
            }
        }
        else
        {
            operationRequests_status = operationRequests;
        }

        return operationRequests_status;
    }
    catch (Exception e)
    {
        // Log the exception or handle it as needed
        throw new ApplicationException($"An error occurred while searching for operation requests: {e.Message}", e);
    }
}
    }
}