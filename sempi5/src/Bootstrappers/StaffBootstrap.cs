﻿using Sempi5.Domain.PersonalData;
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

        var specialization1 = new Specialization(new SpecializationName("Cardiology"));
        var specialization2 = new Specialization(new SpecializationName("Surgeon"));

        var nurse = new Staff
        (
            nurseUser,
            new LicenseNumber(124),
            new Person(new Name("Jane"), new Name("Smith"), new ContactInfo(new Email("nurse@example.com"), new PhoneNumber(988654321))),
            specialization1,
            StaffStatusEnum.INACTIVE
        );

        var doctor = new Staff
        (
            doctorUser,
            new LicenseNumber(125),
            new Person(new Name("Alice"), new Name("Johnson"), new ContactInfo(new Email("admin@example.com"), new PhoneNumber(977654321))),
            specialization2,
            StaffStatusEnum.ACTIVE
        );

        var test = new Staff(
            doctorTest,
            new LicenseNumber(129),
            new Person(new Name("Rute"), new Name("Maia"),
                new ContactInfo(new Email("test@test.com"), new PhoneNumber(966652994))),
            specialization1,
            StaffStatusEnum.ACTIVE);

        await _staffRepository.AddAsync(nurse);
        await _staffRepository.AddAsync(doctor);
        await _staffRepository.AddAsync(test);
    }

    public async Task SeedStaffProfiles()
    {
        var orthopedicSurgeon = new Specialization(new SpecializationName("Orthopedic Surgeon"));
        var generalSurgeon = new Specialization(new SpecializationName("General Surgeon"));
        var doctorSpecialization = new Specialization(new SpecializationName("Doctor"));
        var nurseSpecialization = new Specialization(new SpecializationName("Nurse"));
        
        var orthopedicSurgeonStaff = new Staff
        (
            null,
            new LicenseNumber(126),
            new Person(new Name("Frank"), new Name("Sinatra"), new ContactInfo(new Email("frankie@example.com"), new PhoneNumber(977654621))),
            orthopedicSurgeon,
            StaffStatusEnum.ACTIVE
        );

        var generalSurgeonStaff = new Staff
        (
            null,
            new LicenseNumber(127),
            new Person(new Name("Michael"), new Name("Jackson"), new ContactInfo(new Email("mickel@test.com"), new PhoneNumber(907654621))),
            generalSurgeon,
            StaffStatusEnum.ACTIVE
        );

        var doctorStaff = new Staff
        (
            null,
            new LicenseNumber(217),
            new Person(new Name("John"), new Name("Stewart"), new ContactInfo(new Email("john@example.com"), new PhoneNumber(989898765))),
            doctorSpecialization,
            StaffStatusEnum.ACTIVE
        );

        var nurseStaff = new Staff
        (
            null,
            new LicenseNumber(218),
            new Person(new Name("Alice"), new Name("Johnson"), new ContactInfo(new Email("alice@example.com"), new PhoneNumber(987987985))),
            nurseSpecialization,
            StaffStatusEnum.ACTIVE
        );


        await _staffRepository.AddAsync(orthopedicSurgeonStaff);
        await _staffRepository.AddAsync(generalSurgeonStaff);
        await _staffRepository.AddAsync(doctorStaff);
        await _staffRepository.AddAsync(nurseStaff);
    }
}