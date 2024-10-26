﻿using System;
using Sempi5.Domain.RequiredStaffAggregate;
using Sempi5.Domain.SpecializationAggregate;
using Xunit;
using Assert = Xunit.Assert;

namespace Sempi5.Tests.Domain.RequiredStaffAggregate.Integration;

public class RequiredStaffTest
{
    [Fact]
    public void TestConstructorWithValidParameters()
    {
        // Arrange
        var numStaff = new NumberOfStaff(1);
        var specializationName = new SpecializationName("Test");
        var specialization = new Specialization(specializationName);

        // Act
        var obj = new RequiredStaff(numStaff, specialization);

        // Assert
        Assert.NotNull(obj);
    }

    [Fact]
    public void TestConstructorWithNullNumberOfStaff()
    {
        // Arrange
        var specializationName = new SpecializationName("Test");
        var specialization = new Specialization(specializationName);

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new RequiredStaff(null, specialization));
    }

    [Fact]
    public void TestConstructorWithNullSpecialization()
    {
        // Arrange
        var numStaff = new NumberOfStaff(1);

        // Act & Assert
        Assert.Throws<ArgumentNullException>(() => new RequiredStaff(numStaff, null));
    }

    [Fact]
    public void TestEquals()
    {
        // Arrange
        var numStaff1 = new NumberOfStaff(1);
        var numStaff2 = new NumberOfStaff(1);
        var specializationName = new SpecializationName("Test");
        var specialization1 = new Specialization(specializationName);
        var specialization2 = new Specialization(specializationName);

        var obj1 = new RequiredStaff(numStaff1, specialization1);
        var obj2 = new RequiredStaff(numStaff2, specialization2);

        // Act & Assert
        Assert.True(obj1.Equals(obj2));
    }
    
    [Fact]
    public void TestEqualsWithDifferentNumberOfStaff()
    {
        // Arrange
        var numStaff1 = new NumberOfStaff(1);
        var numStaff2 = new NumberOfStaff(2);
        var specializationName = new SpecializationName("Test");
        var specialization = new Specialization(specializationName);

        var obj1 = new RequiredStaff(numStaff1, specialization);
        var obj2 = new RequiredStaff(numStaff2, specialization);

        // Act & Assert
        Assert.False(obj1.Equals(obj2));
    }

    [Fact]
    public void TestEqualsWithDifferentSpecialization()
    {
        // Arrange
        var numStaff = new NumberOfStaff(1);
        var specializationName1 = new SpecializationName("Test1");
        var specializationName2 = new SpecializationName("Test2");
        var specialization1 = new Specialization(specializationName1);
        var specialization2 = new Specialization(specializationName2);

        var obj1 = new RequiredStaff(numStaff, specialization1);
        var obj2 = new RequiredStaff(numStaff, specialization2);

        // Act & Assert
        Assert.False(obj1.Equals(obj2));
    }

    [Fact]
    public void TestEqualsWithNull()
    {
        // Arrange
        var numStaff = new NumberOfStaff(1);
        var specializationName = new SpecializationName("Test");
        var specialization = new Specialization(specializationName);

        var obj1 = new RequiredStaff(numStaff, specialization);

        // Act & Assert
        Assert.False(obj1.Equals(null));
    }
}
