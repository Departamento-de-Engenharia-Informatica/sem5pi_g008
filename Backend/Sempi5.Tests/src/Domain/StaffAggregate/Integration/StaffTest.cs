//TODO: Fix the tests

/*
using System;
using Sempi5.Domain.PersonalData;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Domain.StaffAggregate;
using Sempi5.Domain.User;
using Xunit;
using Assert = Xunit.Assert;

namespace Sempi5.Tests.Domain.StaffAggregate.Integration;

public class StaffTest
{
    [Theory]
    [InlineData(123, "John", "Doe", "teste@gmail.com", 987654321, "Cardiology")]
    [InlineData(2134, "Jane", "Doe", "teste2@gmail.com", 923456780, "Nurse")]
    public void Constructor_ShouldInitializeFieldsCorrectly_WithPersonObject(
        int licenseNumber, string firstName, string lastName, string email, int phoneNumber, string specialization)
    {
        // Arrange
        var licenseNum = new LicenseNumber(licenseNumber);
        var specializationName = new SpecializationName(specialization);
        var specializationObj = new Specialization(specializationName);
        var first = new Name(firstName);
        var last = new Name(lastName);
        var emailObj = new Email(email);
        var phone = new PhoneNumber(phoneNumber);
        var contactInfo = new ContactInfo(emailObj, phone);
        var person = new Person(first, last, contactInfo);

        // Act
        var staff = new Staff(licenseNum, person, specializationObj);

        // Assert
        Assert.NotNull(staff);
        Assert.Null(staff.User);
        Assert.Equal(licenseNum, staff.LicenseNumber);
        Assert.Equal(specializationObj, staff.Specialization);
        Assert.Equal(person, staff.Person);
        Assert.Equal(StaffStatusEnum.INACTIVE, staff.Status);
        Assert.Empty(staff.AvailabilitySlots);
    }
    
    [Theory]
    [InlineData(123,"John", "Doe", "teste@gmail.com", 987654321)]
    [InlineData(213, "Jane", "Doe", "teste2@gmail.com", 923456780)]
    public void ConstructorWithSpecializationNull(int licenseNumber,string firstName, string lastName, string email, int phoneNumber)
    {
        var license = new LicenseNumber(licenseNumber);
        var first = new Name(firstName);
        var last = new Name(lastName);
        var emailAdd = new Email(email);
        var phone = new PhoneNumber(phoneNumber);
        var contactInfo = new ContactInfo(emailAdd, phone);
        var person = new Person(first, last, contactInfo);
        
        Assert.Throws<ArgumentNullException>(() => new Staff(license, person, null));
    }
    
    [Theory]
    [InlineData(123,"Cardiology")]
    [InlineData(213, "Doctor")]
    public void ConstructorWithPersonNull(int licenseNumber, string specialization)
    {
        var license = new LicenseNumber(licenseNumber);
        var specializationName = new SpecializationName(specialization);
        var specializa = new Specialization(specializationName);
        
        Assert.Throws<ArgumentNullException>(() => new Staff(license, null, specializa));
    }
    
    [Theory]
    [InlineData("John", "Doe", "teste@gmail.com", 987654321, "Doctor")]
    [InlineData("Jane", "Doe", "teste2@gmail.com", 923456780, "Cardiology")]
    public void ConstructorWithLicenseNumberNull(string firstName, string lastName, string email, int phoneNumber, string specialization)
    {
        var firstNameObj = new Name(firstName);
        var lastNameObj = new Name(lastName);
        var emailObj = new Email(email);
        var phoneNumberObj = new PhoneNumber(phoneNumber);
        var contactInfo = new ContactInfo(emailObj, phoneNumberObj);
        var person = new Person(firstNameObj, lastNameObj, contactInfo);
        var specializationName = new SpecializationName(specialization);
        var specializationObj = new Specialization(specializationName);

        Assert.Throws<ArgumentNullException>(() => new Staff(null, person, specializationObj));
    }

    
    [Fact]
    public void ConstructorWithAllNull()
    {
        
        Assert.Throws<ArgumentNullException>(() => new Staff(null, null, null));
    }

    [Theory]
    [InlineData("teste1@gmail.com", "Staff", 123, "John", "Doe", "teste@gmail.com", 987654321, "Cardiology")]
    [InlineData("teste2@gmail.com", "Staff", 2134, "Jane", "Doe", "teste2@gmail.com", 923456780, "Nurse")]
    public void AddUser_ShouldSetUserProperty(
        string emailUser, string role, int licenseNumber, string firstName, string lastName, string email, int phoneNumber, string specialization)
    {
        // Arrange
        var licenseNum = new LicenseNumber(licenseNumber);
        var specializationName = new SpecializationName(specialization);
        var specializationObj = new Specialization(specializationName);
        var first = new Name(firstName);
        var last = new Name(lastName);
        var emailObj = new Email(email);
        var phone = new PhoneNumber(phoneNumber);
        var contactInfo = new ContactInfo(emailObj, phone);
        var person = new Person(first, last, contactInfo);

        var staff = new Staff(licenseNum, person, specializationObj);

        var newUser = new SystemUser(new Email(emailUser), role);

        // Act
        staff.AddUser(newUser);

        // Assert
        Assert.Equal(newUser, staff.User);
    }
}
*/

