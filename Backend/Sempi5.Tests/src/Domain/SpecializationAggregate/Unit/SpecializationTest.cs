//TODO: Fix the tests

/*using System;
using JetBrains.Annotations;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Moq;
using Sempi5.Domain.SpecializationAggregate;
using Xunit;
using Assert = Xunit.Assert;

namespace Sempi5.Tests.Domain.SpecializationAggregate.Unit;


public class SpecializationTest
{

    [Fact]
    public void ConstructorTest()
    {
        
        var specializationNameMock = new Mock<SpecializationName>("Cardiology");
        
        specializationNameMock.SetupGet(x => x.name).Returns("Cardiology");
        
        var obj = new Specialization(specializationNameMock.Object);
        
        Assert.NotNull(obj);
        Assert.Equal(specializationNameMock.Object, obj.specializationName);
    }
    
    [Fact]
    public void ConstructorWithNullSpecializationNameTest()
    {
        Assert.Throws<ArgumentNullException>(() => new Specialization(null));
    }
    
    [Fact]
    public void EqualsTest()
    {
        var name = "Cardiology";
        
        var specializationName1 = new Mock<SpecializationName>(name);
        var specializationName2 = new Mock<SpecializationName>(name);
        
        specializationName1.SetupGet(x => x.name).Returns(name);
        specializationName2.SetupGet(x => x.name).Returns(name);
        
        var obj1 = new Specialization(specializationName1.Object);
        var obj2 = new Specialization(specializationName2.Object);
        
        Assert.True(obj1.Equals(obj2));
    }
    
    [Theory]
    [InlineData("Cardiology", "Neurology")]
    [InlineData("Operator", "Doctor")]
    public void NotEqualsTest(string name1, string name2)
    {
        var specializationName1 = new Mock<SpecializationName>(name1);
        var specializationName2 = new Mock<SpecializationName>(name2);
        
        specializationName1.SetupGet(x => x.name).Returns(name1);
        specializationName2.SetupGet(x => x.name).Returns(name2);
        
        specializationName1.Setup(x => x.ToString()).Returns(name1);
        specializationName2.Setup(x => x.ToString()).Returns(name2);
        
        var obj1 = new Specialization(specializationName1.Object);
        var obj2 = new Specialization(specializationName2.Object);
        
        Assert.False(obj1.Equals(obj2));
    }
    
}*/