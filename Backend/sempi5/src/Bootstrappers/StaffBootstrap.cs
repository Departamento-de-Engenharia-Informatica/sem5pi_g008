using Sempi5.Domain.PersonalData;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Domain.StaffAggregate;
using Sempi5.Domain.User;
using Sempi5.Infrastructure.StaffAggregate;
using Sempi5.Infrastructure.StaffRepository;
using Sempi5.Infrastructure.UserRepository;

namespace Sempi5.Bootstrappers;

public class StaffBootstrap
{
    private readonly IStaffRepository _staffRepository;

    public StaffBootstrap(IStaffRepository staffRepo)
    {
        _staffRepository = staffRepo;
    }

    public async Task SeedActiveStaff()
    {
        var nurseUser = new SystemUser(new Email("nurse@example.com"), "Nurse", true);
        var doctorUser = new SystemUser(new Email("admin@example.com"), "Doctor", true);
        var doctorTest = new SystemUser(new Email("rutemaia2004@gmail.com"), "Doctor", true);
        var nurseUserRui = new SystemUser(new Email("rpsoares8@gmail.com"), "Nurse", true);
        var doctorUserRui2 = new SystemUser(new Email("swag4yt@gmail.com"), "Doctor", true);
        var nurseUserRui2 = new SystemUser(new Email("rui.soares13749@aesps.pt"), "Nurse", true);
        var sandroUserDoctor = new SystemUser(new Email("carmoluis28@gmail.com"), "Doctor", true);
        var ruteUserDoctor = new SystemUser(new Email("rutemaia2004@gmail.com"), "Doctor", true);

        
        var specialization1 = new Specialization(new SpecializationName("Cardiology"), new SpecializationCode("CA1"), new SpecializationDescription("Cardiology specialization"));
        var specialization2 = new Specialization(new SpecializationName("Surgeon"), new SpecializationCode("S0"), new SpecializationDescription("Surgeon specialization"));
        var specialization3 = new Specialization(new SpecializationName("Neurologist"), new SpecializationCode("N0"), new SpecializationDescription("Neurologist specialization"));

        var agenda1 = new StaffAgenda
        {
            Date = new DateTime(2024, 12, 16),
            TimeIntervals = new List<string> { "09:00-10:00", "10:00-11:00", "11:00-12:00" },
            appointmentType = "Consultation"
        };


        var nurse = new Staff
        (
            nurseUser,
            new LicenseNumber(124),
            new Person(new Name("Jane"), new Name("Smith"),
                new ContactInfo(new Email("nurse@example.com"), new PhoneNumber(988654321))),
            specialization1,
            StaffStatusEnum.INACTIVE,
            new List<StaffAgenda> { agenda1 },
            new List<string> { "20241216-09:00-23:00" }
        );

        var doctor = new Staff
        (
            doctorUser,
            new LicenseNumber(125),
            new Person(new Name("Alice"), new Name("Johnson"),
                new ContactInfo(new Email("admin@example.com"), new PhoneNumber(977654321))),
            specialization2,
            StaffStatusEnum.ACTIVE,
            new List<StaffAgenda> { agenda1 },
            new List<string> { "20241216-09:00-23:00" }

        );

        var test = new Staff(
            doctorTest,
            new LicenseNumber(129),
            new Person(new Name("Rute"), new Name("Maia"),
                new ContactInfo(new Email("rutemaia2004@gmail.com"), new PhoneNumber(966652994))),
            specialization1,
            StaffStatusEnum.ACTIVE,
            new List<StaffAgenda> { agenda1 },
            new List<string> { "20241216-09:00-23:00" }
        );

    var nurseRui = new Staff
        (
            nurseUserRui,
            new LicenseNumber(200),
            new Person(new Name("Rui"), new Name("Soares"),
                new ContactInfo(new Email("rpsoares8@gmail.com"), new PhoneNumber(964666298))),
            specialization1,
            StaffStatusEnum.ACTIVE,
            new List<StaffAgenda> { agenda1 },
            new List<string> { "20241216-09:00-23:00" }
        );

        var nurseRui2 = new Staff
        (
            nurseUserRui2,
            new LicenseNumber(201),
            new Person(new Name("Rui"), new Name("Soares"),
                new ContactInfo(new Email("rui.soares13749@aesps.pt"), new PhoneNumber(964666299))),
            specialization1,
            StaffStatusEnum.ACTIVE,
            new List<StaffAgenda> { agenda1 },
            new List<string> { "20241216-09:00-23:00" }
        );

        var doctorRui2 = new Staff
        (
            doctorUserRui2,
            new LicenseNumber(202),
            new Person(new Name("Rui"), new Name("Soares"),
                new ContactInfo(new Email("swag4yt@gmail.com"), new PhoneNumber(964666300))),
            specialization2,
            StaffStatusEnum.ACTIVE,
            new List<StaffAgenda> { agenda1 },
            new List<string> { "20241216-09:00-23:00" }
        );
        var doctorSandro = new Staff
        (
            sandroUserDoctor,
            new LicenseNumber(203),
            new Person(new Name("Sandro"), new Name("Doutor"),
                new ContactInfo(new Email("carmoluis28@gmail.com"), new PhoneNumber(938536400))),
            specialization2,
            StaffStatusEnum.ACTIVE,
            new List<StaffAgenda> { agenda1 },
            new List<string> { "20241216-09:00-23:00" }
        );
  
        
        await _staffRepository.AddAsync(doctorSandro);
        await _staffRepository.AddAsync(nurse);
        await _staffRepository.AddAsync(doctor);
        await _staffRepository.AddAsync(test);
        await _staffRepository.AddAsync(nurseRui);
        await _staffRepository.AddAsync(nurseRui2);
        await _staffRepository.AddAsync(doctorRui2);

    }

    public async Task SeedStaffProfiles()
    {
        var orthopedicSurgeon = new Specialization(new SpecializationName("Orthopedic Surgeon"), new SpecializationCode("OS1"), new SpecializationDescription("Orthopedic Surgeon specialization"));
        var generalSurgeon = new Specialization(new SpecializationName("General Surgeon"), new SpecializationCode("GS1"), new SpecializationDescription("General Surgeon specialization"));
        var doctorSpecialization = new Specialization(new SpecializationName("Doctor"), new SpecializationCode("D1"), new SpecializationDescription("Doctor specialization"));
        var nurseSpecialization = new Specialization(new SpecializationName("Nurse"), new SpecializationCode("N5"), new SpecializationDescription("Nurse specialization"));
        
        var agenda1 = new StaffAgenda
        {
            Date = new DateTime(2024, 12, 16),
            TimeIntervals = new List<string> { "09:00-10:00", "10:00-11:00", "11:00-12:00" },
            appointmentType = "Consultation"
        };
        var agenda2 = new StaffAgenda
        {
            Date = new DateTime(2024, 12, 16),
            TimeIntervals = new List<string> { "09:00-10:00", "10:00-11:00", "11:00-12:00" },
            appointmentType = "Consultation"
        };
        
        var orthopedicSurgeonStaff = new Staff
        (
            null,
            new LicenseNumber(126),
            new Person(new Name("Frank"), new Name("Sinatra"), new ContactInfo(new Email("frankie@example.com"), new PhoneNumber(977654621))),
            orthopedicSurgeon,
            StaffStatusEnum.ACTIVE,
            new List<StaffAgenda> { agenda1 },
            new List<string> { "20241216-09:00-23:00" });

        var generalSurgeonStaff = new Staff
        (
            null,
            new LicenseNumber(127),
            new Person(new Name("Michael"), new Name("Jackson"), new ContactInfo(new Email("mickel@test.com"), new PhoneNumber(907654621))),
            generalSurgeon,
            StaffStatusEnum.ACTIVE,
            new List<StaffAgenda> { agenda1,agenda2 },
            new List<string> { "20241216-09:00-23:00" }
        );

        var doctorStaff = new Staff
        (
            null,
            new LicenseNumber(217),
            new Person(new Name("John"), new Name("Stewart"), new ContactInfo(new Email("john@example.com"), new PhoneNumber(989898765))),
            doctorSpecialization,
            StaffStatusEnum.ACTIVE,
            
            new List<StaffAgenda> { agenda2 },
            new List<string> { "20241216-09:00-23:00" }
        );

        var nurseStaff = new Staff
        (
            null,
            new LicenseNumber(218),
            new Person(new Name("Alice"), new Name("Johnson"), new ContactInfo(new Email("alice@example.com"), new PhoneNumber(987987985))),
            nurseSpecialization,
            StaffStatusEnum.ACTIVE,
            new List<StaffAgenda> { agenda1 },
            new List<string> { "20241216-09:00-23:00" }
        );


        await _staffRepository.AddAsync(orthopedicSurgeonStaff);
        await _staffRepository.AddAsync(generalSurgeonStaff);
        await _staffRepository.AddAsync(doctorStaff);
        await _staffRepository.AddAsync(nurseStaff);
    }
}