using System.Security.Claims;
using System.Text.Json;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using Microsoft.DotNet.Scaffolding.Shared.Messaging;
using Sempi5.Domain.PatientAggregate;
using Sempi5.Domain.PatientAggregate.Exceptions;
using Sempi5.Domain.Shared;
using Sempi5.Domain.StaffAggregate.StaffExceptions;
using Sempi5.Services;
using ILogger = Serilog.ILogger;

namespace Sempi5.Controllers;

[ApiController]
[Route("[controller]")]
public class PatientController : ControllerBase
{
    private readonly PatientService patientService;
    private readonly EmailService emailService;
    private readonly Serilog.ILogger _logger;

    public PatientController(PatientService patientService, EmailService emailService, ILogger logger)
    {
        this.patientService = patientService;
        this.emailService = emailService;
        _logger = logger;
    }

    [HttpGet("checkUserToDelete")]
    public async Task<IActionResult> checkUserToDelete()
    {
        try
        {
            await patientService.checkUserToDelete();
            return Ok(new {message="Pendent Users were deleted"});
        }
        catch (Exception ex)
        {
            return BadRequest("An error occurred while processing your request. " + ex.Message);
        }
    }


    private string getEmail()
    {
        var claimsIdentity = User.Identity as ClaimsIdentity;
        return claimsIdentity?.FindFirst(ClaimTypes.Email).Value;
    }

    [HttpGet("register")]
    [Authorize(Roles = "Unregistered")]
    public IActionResult Register()
    {
        return Ok("Por favor, forneça o seu número.");
    }

    [HttpPost("register")]
    [Authorize(Roles = "Unregistered")]
    //TODO - Use email from the cookies (claim principal)
    public async Task<IActionResult> RegisterNumber(int number)
    {
        try
        {
            if (number <= 0)
            {
                return BadRequest("Número de registro não pode ser vazio ou negativo.");
            }

            var success = await patientService.RegisterPatientUser(getEmail(), number);

            if (success)
            {
                return Ok(new
                    { messge = $"Número de registro {number} registrado com sucesso para o email: {getEmail()}." });
            }
            else
            {
                return BadRequest("Erro ao registrar número.");
            }
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }

    [HttpGet("account/appointment")]
    [Authorize(Roles = "Patient")]
    public async Task<IActionResult> listAppointments()

    {
        var appointments = await patientService.appointmentList(getEmail());
        if (appointments == null)
        {
            return BadRequest("Unauthorized acess(you need to confirm your account)");
        }

        return Ok(appointments);
    }


    [HttpDelete("account/exclude")]
    [Authorize(Roles = "Patient")]
    public async Task<IActionResult> excludeAccount()
    {
        await patientService.defineDataToExcludeAccount(getEmail());

        return Ok(new { message = "We have sent an email to confirm the exclusion." });
    }

    [HttpGet("account/exclude/confirm/{token}")]
    [Authorize(Roles = "Patient")]
    public async Task<IActionResult> excludeAccountEmailConfirm(string token)
    {
        try
        {
            await patientService.excludeAccountSchedule(token);
            return Ok("Your accoount will be deleted in 30 days");
        }
        catch (ArgumentException ex)
        {
            // token inválido
            return BadRequest("Token inválido: " + ex.Message);
        }
        catch (InvalidOperationException ex)
        {
            // token não encontrado ou usuário não encontrado
            return NotFound("Erro ao excluir a conta: " + ex.Message);
        }
        catch (Exception ex)
        {
            // erro inesperado
            return StatusCode(500, "Erro interno do servidor. Tente novamente mais tarde." + ex.Message);
        }
    }


    [HttpPatch("account/update")]
    [Authorize(Roles = "Patient")]
    public async Task<IActionResult> updateAccount(PatientProfileDto profileDto)
    {
        //criar um token para eter no link
        if (profileDto.email != null || profileDto.phoneNumber != null)
        {
            string serializedDto = JsonSerializer.Serialize(profileDto);

            await SendUpdateConfirmationEmail(getEmail(),
                $"http://localhost:5001/patient/account/update/{serializedDto}", "Update Confirmation");
            return Ok(new { message = "Email sent to confirm update." });
        }

        //adicionar um novo parametro que é o email do usuario logado
        await patientService.updateAccount(profileDto, getEmail());
        return Ok(new { message = "Account updated." });
    }

    [HttpGet("account/update/{jsonString}")]
    public async Task<IActionResult> updateAccounlt(string jsonString)
    {
        PatientProfileDto profileDto;
        try
        {
            profileDto = JsonSerializer.Deserialize<PatientProfileDto>(jsonString);
        }
        catch (JsonException ex)
        {
            // Handle deserialization error
            return BadRequest($"Invalid JSON format: {ex.Message}");
        }


        await patientService.updateAccount(profileDto, getEmail());
        return Ok(new { message = "Account updated" });
    }

    private async Task SendUpdateConfirmationEmail(string email, string link, string subject)
    {
        var body = $@"
        <p>Dear user,</p>
        <p>Please confirm this email by clicking the following link:</p>
        <a href='{link}'>Confirm Email</a>
        <p>If you did not request this, please ignore this email.</p>";

        await emailService.SendEmailAsync(email, body, subject);
    }

    [HttpGet("listPatientProfilesByName/{name}")]
    [Authorize(Roles = "Admin")]
    public async Task<IActionResult> ListPatientProfilesByName(string name)
    {
        try
        {
            var nameDto = new NameDTO { name = name };
            var patientProfile = await patientService.ListPatientByName(nameDto);

            return Ok(patientProfile);
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }

    [HttpGet("listPatientProfilesByEmail/{email}")]
    [Authorize(Roles = "Admin")]
    public async Task<IActionResult> ListPatientProfilesByEmail(string email)
    {
        try
        {
            var emailDto = new EmailDTO { email = email };
            var patientProfiles = await patientService.ListPatientByEmail(emailDto);
            return Ok(patientProfiles);
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }

    [HttpGet("listPatientProfilesByMedicalRecordNumber/{id}")]
    [Authorize(Roles = "Admin")]
    public async Task<IActionResult> ListPatientProfilesByMedicalRecordNumber(
        string Id)
    {
        try
        {
            var medicalRecordNumberDto = new PatientIdDto { Id = Id };
            var patientProfiles = await patientService.ListPatientByMedicalRecordNumber(medicalRecordNumberDto);
            return Ok(patientProfiles);
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }

    [HttpGet("listPatientProfilesByDateOfBirth/{birthDate}")]
    [Authorize(Roles = "Admin")]
    public async Task<IActionResult> ListPatientProfilesByDateOfBirth(string birthDate)
    {
        try
        {
            string[] parts = birthDate.Split('-');
            int year = int.Parse(parts[0]);
            int month = int.Parse(parts[1]);
            int day = int.Parse(parts[2]);
            var dateDto = new DateDTO { year = year, month = month, day = day };
            var patientProfiles = await patientService.ListPatientByDateOfBirth(dateDto);
            return Ok(patientProfiles);
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }

    [HttpDelete("deletePatientProfile/{email}")]
    public async Task<IActionResult> DeletePatientProfile(string email)
    {
        try
        {
            await patientService.DeletePatientProfile(email);
            return Ok("Patient deleted successfully");
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }

    [HttpPut("editPatientProfile/{email}")]
    public async Task<IActionResult> EditPatientProfile([FromBody] PatientDTO patientDto, string email)
    {
        try
        {
            await patientService.EditPatientProfile(patientDto, email);
            _logger.ForContext("CustomLogLevel", "CustomLevel")
                .Information($"\nChanges To Patient {patientDto.patientId} :" +
                             $" {patientDto.phoneNumber}" +
                             $" {patientDto.email}" +
                             $" {patientDto.firstName}" +
                             $" {patientDto.lastName}" +
                             $" {patientDto.birthDate} \n");
            return Ok("Patient profile edited successfully");
        }
        catch (Exception e)
        {
            return BadRequest(e.Message + e.StackTrace);
        }
    }


    [HttpPost("registerPatientProfile")]
    public async Task<IActionResult> RegisterPatientProfile(PatientDTO patient)
    {
        try
        {
            await patientService.CreatePatientProfile(patient);
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
        
        try
        {
            var authenticationCookie = ".AspNetCore.Cookies=" + HttpContext.Request.Cookies[".AspNetCore.Cookies"];
             await patientService.CreatePatientMedicalRecord(authenticationCookie, patient.phoneNumber);
        }
        catch (Exception e)
        {
            return Conflict(e.Message);
        }
        
        return Ok("Patient profile created successfully");
    }


    [HttpGet]
    [Authorize(Roles = "Admin,Doctor")]
    public async Task<IActionResult> ListAllActivePatientProfiles()
    {
        try
        {
            var patientProfile = await patientService.ListAllActivePatients();

            return Ok(patientProfile);
        }
        catch (PatientsProfilesNotFoundException e)
        {
            return BadRequest(e.Message);
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }
    
    
    [HttpGet("listAllActivePatientProfilesNames")]
    [Authorize(Roles = "Doctor")]
    public async Task<IActionResult> ListAllActivePatientProfilesNames()
    {
        try
        {
            var patientProfile = await patientService.ListAllActivePatientsNames();

            return Ok(patientProfile);
        }
        catch (PatientsProfilesNotFoundException e)
        {
            return BadRequest(e.Message);
        }
        catch (Exception e)
        {
            return BadRequest(e.Message);
        }
    }
    
    [HttpGet("profile")]
    [Authorize(Roles = "Patient")]

    public async Task<IActionResult> SearchRequests()
        {
            try
            {
                 var patient = await patientService.GetPatientProfile(getEmail());
                

                    var tableData = new List<object>(); 
                    tableData.Add(new
                        { 
                            Email=patient.Person?.ContactInfo.email().ToString(),
                            PhoneNumber=patient.Person?.ContactInfo._phoneNumber.phoneNumber().ToString(),
                            Name=patient.Person?.FullName._name,
                            BirthDate=patient.BirthDate,
                            Gender=patient.Gender,
                            EmergencyContact=patient.EmergencyContact,
                            Allergies=patient.AllergiesAndMedicalConditions,
                            AppointmentHistory=patient.AppointmentHistory
                        }
                    );
                    
                    return Ok( tableData );
                    
            }
            catch (Exception e)
            {
                return BadRequest($"Error: {e.Message}");
            }
        } 
}