using Sempi5.Domain.OperationRequestAggregate;
using Sempi5.Domain.OperationTypeAggregate;
using Sempi5.Domain.PatientAggregate;
using Sempi5.Domain.PersonalData;
using Sempi5.Domain.RequiredStaffAggregate;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Domain.StaffAggregate;
using Sempi5.Domain.User;
using Sempi5.Infrastructure.OperationRequestAggregate;


using Sempi5.Domain.AppointmentAggregate;
using Sempi5.Domain.SurgeryRoomAggregate;
using Sempi5.Infrastructure.AppointmentAggregate;


namespace Sempi5.Bootstrappers;

public class OperationRequestBootstrap
{
    private readonly IOperationRequestRepository _operationRequestRepository;
    private readonly IAppointmentRepository _appointmentRepository;


    public OperationRequestBootstrap(IOperationRequestRepository operationRequestRepo, IAppointmentRepository appointmentRepository)
    {
        _operationRequestRepository = operationRequestRepo;
        _appointmentRepository = appointmentRepository;
    }

    public async Task SeedOperationRequests()
    {
        var user1 = new SystemUser(new Email("newUser@gmail.com"), "Patient", true);

        var patient1 = new Patient
        (
            user1, 
            new Person(new Name("new"), new Name("User"),
                new ContactInfo(new Email("anotherEmail@gmail.com"), new PhoneNumber(987654322))),
            new DateTime(1990, 1, 10),
            "Female",
            new List<string> { "Peanuts", "Asthma" },
            "456",
            new List<string> { "01/01/2021 9am-10am", "02/02/2021 10am-11am" },
            PatientStatusEnum.ACTIVATED
        );
        
        var specializationName = new SpecializationName("Cleaner");
        var specializationCode = new SpecializationCode("C1");
        var specializationDescription = new SpecializationDescription("Especialização em Cardiologia");
        var specialization1 = new Specialization(specializationName, specializationCode, specializationDescription);
        
        var specializationName2 = new SpecializationName("Heart Surgeon");
        var specializationCode2 = new SpecializationCode("HS1");
        var specializationDescription2 = new SpecializationDescription("Especialização em Cardiologia");
        var specialization2 = new Specialization(specializationName2, specializationCode2, specializationDescription2);

        
        var requiredStaff1 = new RequiredStaff(new NumberOfStaff(3), specialization1);
        var requiredStaff2 = new RequiredStaff(new NumberOfStaff(1), specialization2);
        
        var time11 = TimeSpan.Parse("03:00:00");
        var time12 = TimeSpan.Parse("02:00:00");
        var time13 = TimeSpan.Parse("01:00:00");

        var operationType1 = new OperationType(new OperationName("Heart Surgery"),
            new List<RequiredStaff> { requiredStaff1, requiredStaff2 }, time11, time12, time13);

        var doctorUser = new SystemUser(new Email("doctor@gmail.com"), "Doctor", true);
        
        var doctor = new Staff
        (
            doctorUser,
            new LicenseNumber(213),
            new Person(new Name("Johnnnnn"), new Name("Doe"), new ContactInfo(new Email("doctor@example.com"), new PhoneNumber(987254321))),
            specialization1,
            StaffStatusEnum.ACTIVE
        );

        var request1 = new OperationRequest(doctor, patient1, operationType1, new DateTime(2021, 1, 1), PriorityEnum.HIGH);
        
        await _operationRequestRepository.AddAsync(request1);
        
        var user2 = new SystemUser(new Email("exampleUser@gmail.com"), "Patient", true);
        var patient2 = new Patient
        (
            user2, 
            new Person(new Name("Alice"), new Name("Smith"),
                new ContactInfo(new Email("alice_smith@example.com"), new PhoneNumber(912345678))),
            new DateTime(1985, 5, 20),
            "Female",
            new List<string> { "Gluten", "Penicillin" },
            "789",
            new List<string> { "15/03/2021 8am-9am", "18/04/2021 10am-11am" },
            PatientStatusEnum.ACTIVATED
        );
        
        var specializationName3 = new SpecializationName("Neurologist");
        var specializationCode3 = new SpecializationCode("N1");
        var specializationDescription3 = new SpecializationDescription("Especialização em Cardiologia");
        var specialization3 = new Specialization(specializationName3, specializationCode3, specializationDescription3);
        
        var specializationName4 = new SpecializationName("Anesthesiologist");
        var specializationCode4 = new SpecializationCode("A1");
        var specializationDescription4 = new SpecializationDescription("Especialização em Cardiologia");
        var specialization4 = new Specialization(specializationName4, specializationCode4, specializationDescription4);

        var requiredStaff3 = new RequiredStaff(new NumberOfStaff(2), specialization3);
        var requiredStaff4 = new RequiredStaff(new NumberOfStaff(1), specialization4);

        var time21 = TimeSpan.Parse("04:00:00");
        var time22 = TimeSpan.Parse("02:30:00");
        var time23 = TimeSpan.Parse("01:30:00");

        var operationType2 = new OperationType(new OperationName("Brain Surgery"),
            new List<RequiredStaff> { requiredStaff3, requiredStaff4 }, time21, time22, time23);

        var doctorUser2 = new SystemUser(new Email("neuro_doctor@gmail.com"), "Doctor", true);

        var doctor2 = new Staff
        (
            doctorUser2,
            new LicenseNumber(987),
            new Person(new Name("Michael"), new Name("Anderson"), new ContactInfo(new Email("michael.anderson@example.com"), new PhoneNumber(912345679))),
            specialization3,
            StaffStatusEnum.ACTIVE
        );

        var request2 = new OperationRequest(doctor2, patient2, operationType2,
            new DateTime(2021, 5, 20), PriorityEnum.LOW);

        await _operationRequestRepository.AddAsync(request2);
        
        var room1 =  new SurgeryRoom(RoomTypeEnum.ICU, new RoomCapacity(10), new List<string>(), RoomStatusEnum.AVAILABLE,
            new List<string>());

        var appointment1 = new Appointment(request2,
            room1, new DateTime(2021, 1, 1),
            StatusEnum.NOT_SCHEDULED);
        await _appointmentRepository.AddAsync(appointment1);


    }
}