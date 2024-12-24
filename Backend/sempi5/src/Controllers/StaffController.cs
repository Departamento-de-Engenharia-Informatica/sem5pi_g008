using System.Security.Claims;
using Microsoft.AspNetCore.Mvc;
using System.Text.Json;
using Microsoft.AspNetCore.Authorization;
using Sempi5.Domain.Encrypt;
using Sempi5.Domain.PatientAggregate;
using Sempi5.Domain.PersonalData.Exceptions;
using Sempi5.Domain.Shared.Exceptions;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Domain.SpecializationAggregate.SpecializationExceptions;
using Sempi5.Domain.StaffAggregate;
using Sempi5.Domain.StaffAggregate.DTOs;
using Sempi5.Domain.StaffAggregate.StaffExceptions;
using Sempi5.Services;

namespace Sempi5.Controllers.StaffControllers
{
    [Route("[controller]")]
    [ApiController]
    // [Authorize]
    public class StaffController : ControllerBase
    {
        private readonly StaffService _staffService;
        private readonly Serilog.ILogger _logger;
        private readonly EmailService _emailService;

        public StaffController(StaffService staffService, Serilog.ILogger logger, EmailService emailService)
        {
            _staffService = staffService;
            _logger = logger;
            _emailService = emailService;
        }

        [HttpPost]
        [Authorize(Roles = "Admin")]
        public async Task<IActionResult> CreateStaffProfile(StaffDTO staff)
        {
            try
            {
                await _staffService.CreateStaffProfile(staff);
                return Ok(new { message = "Staff profile created successfully!" });
            }
            catch (InvalidPhoneNumberFormat e)
            {
                return StatusCode(600, e.Message);
            }
            catch (PhoneNumberAlreadyInUseException e)
            {
                return StatusCode(601, e.Message);
            }
            catch (InvalidEmailFormatException e)
            {
                return StatusCode(602, e.Message);
            }
            catch (EmailAlreadyInUseException e)
            {
                return StatusCode(603, e.Message);
            }
            catch (InvalidLicenseNumberFormatException e)
            {
                return StatusCode(604, e.Message);
            }

            catch (LicenseNumberAlreadyInUseException e)
            {
                return StatusCode(605, e.Message);
            }
            catch (Exception e)
            {
                return BadRequest("Error creating Staff:" + e.Message);
            }
        }


        [HttpPatch]
        [Authorize(Roles = "Admin")]
        public async Task<IActionResult> EditStaffProfile([FromBody] EditStaffDTO editStaffDto)
        {
            try
            {
                var staff = await _staffService.GetStaffById(editStaffDto.Id);

                if (editStaffDto.email != null || editStaffDto.phoneNumber > 0)
                {
                    if (editStaffDto.email != null) await _staffService.VerifyEmailAvailability(editStaffDto.email);

                    if (editStaffDto.phoneNumber > 0)
                        await _staffService.VerifyPhoneNumberAvailability(editStaffDto.phoneNumber);


                    await _emailService.PrepareEditStaffConfirmationEmail(staff.Person.ContactInfo._email.ToString(),
                        editStaffDto);
                }
                else
                {
                    await _staffService.EditStaffProfile(editStaffDto);
                    _logger.ForContext("CustomLogLevel", "CustomLevel")
                        .Information($"\nChanges To Staff {editStaffDto.Id} :" +
                                     $" {editStaffDto.phoneNumber}" +
                                     $" {editStaffDto.email}" +
                                     $" {editStaffDto.specialization} \n");

                    return Ok(new { message = "Staff profile edited successfully!" });
                }

                return Ok(new { message = "A confirmation email was sent." });
            }
            catch (InvalidPhoneNumberFormat e)
            {
                return StatusCode(600, e.Message);
            }
            catch (PhoneNumberAlreadyInUseException e)
            {
                return StatusCode(601, e.Message);
            }
            catch (InvalidEmailFormatException e)
            {
                return StatusCode(602, e.Message);
            }
            catch (EmailAlreadyInUseException e)
            {
                return StatusCode(603, e.Message);
            }
            catch (Exception e)
            {
                return BadRequest(e.Message);
            }
        }


        [HttpGet("updateContactInfo/{jsonString}")]
        public async Task<IActionResult> EditStaffProfile(string jsonString)
        {
            EditStaffDTO editStaffDto;
            try
            {
                var cryptography = new Cryptography();
                var encryptedString = cryptography.DecryptString(jsonString);

                editStaffDto = JsonSerializer.Deserialize<EditStaffDTO>(encryptedString);

                await _staffService.EditStaffProfile(editStaffDto);

                _logger.ForContext("CustomLogLevel", "CustomLevel")
                    .Information($"\nChanges To Staff {editStaffDto.Id} :" +
                                 $" {editStaffDto.phoneNumber}" +
                                 $" {editStaffDto.email}" +
                                 $" {editStaffDto.specialization} \n");
            }
            catch (JsonException ex)
            {
                return BadRequest($"Invalid JSON format: {ex.Message}");
            }
            catch (Exception e)
            {
                return BadRequest(e.Message);
            }

            return Ok("Staff profile edited successfully!");
        }

        [HttpDelete("{staffId}")]
        [Authorize(Roles = "Admin")]
        public async Task<IActionResult> DeactivateStaffProfile(string staffId)
        {
            try
            {
                var staffIdDto = new StaffIdDTO { Id = staffId };

                await _staffService.DeactivateStaffProfile(staffIdDto);
                return Ok(new { message = "Staff deactivated successfully." });
            }
            catch (StaffProfilesNotFoundException e)
            {
                return NotFound(e.Message);
            }
            catch (Exception e)
            {
                return BadRequest(e.Message);
            }
        }


        [HttpGet("by-name/{name}")]
        [Authorize(Roles = "Admin")]
        public async Task<IActionResult> ListStaffProfileByName(string name)
        {
            try
            {
                var nameDto = new NameDTO { name = name };

                var staffProfile = await _staffService.ListStaffByName(nameDto);
                return Ok(staffProfile);
            }
            catch (StaffProfilesNotFoundException e)
            {
                return NotFound(e.Message);
            }
            catch (Exception e)
            {
                return BadRequest(e.Message);
            }
        }

        [HttpGet("by-email/{email}")]
        [Authorize(Roles = "Admin")]
        public async Task<IActionResult> ListStaffProfileByEmail(string email)
        {
            try
            {
                var emailDto = new EmailDTO { email = email };

                var staffProfiles = await _staffService.ListStaffByEmail(emailDto);
                return Ok(staffProfiles);
            }
            catch (StaffProfilesNotFoundException e)
            {
                return NotFound(e.Message);
            }
            catch (Exception e)
            {
                return BadRequest(e.Message);
            }
        }

        [HttpGet("by-specialization/{specialization}")]
        [Authorize(Roles = "Admin")]
        public async Task<IActionResult> ListStaffProfilesBySpecialization(string specialization)
        {
            try
            {
                var specializationDto = new SpecializationNameDTO { specializationName = specialization };

                var staffProfile = await _staffService.ListStaffBySpecialization(specializationDto);
                return Ok(staffProfile);
            }
            catch (SpecializationNotFoundException e)
            {
                return StatusCode(403, e.Message);
            }
            catch (StaffProfilesNotFoundException e)
            {
                return NotFound(e.Message);
            }
            catch (Exception e)
            {
                return BadRequest(e.Message);
            }
        }
        
        [HttpGet("by-id/{staffId}")]
        //[Authorize(Roles = "Admin")]
        public async Task<IActionResult> GetStaffById(string staffId)
        {
            try
            {
                var staffDetailsDTO = await _staffService.GetStaffDetailsById(staffId);
                
                return Ok(staffDetailsDTO);
            }
            catch (StaffProfilesNotFoundException e)
            {
                return NotFound(e.Message);
            }
            catch (Exception e)
            {
                return BadRequest(e.Message);
            }
        }
        
        
        [HttpDelete("request/deleteRequest/{requestId}")]
        [Authorize(Roles = "Doctor")]
        public async Task<IActionResult> DeleteRequest(string requestId)
        {
            try
            {
                await _staffService.DeleteRequestAsync(getEmail(), requestId);
                return Ok(new {message="Operation request deleted successfully"});
            }
            catch (UnauthorizedAccessException ex)
            {
                return Unauthorized(ex.Message);
            }
            catch (InvalidOperationException ex)
            {
                return BadRequest(ex.Message);
            }
        }


        [HttpGet]
        [Authorize(Roles = "Admin")]
        public async Task<IActionResult> ListAllStaffProfiles()
        {
            try
            {
                var staffProfile = await _staffService.ListAllStaff();
                return Ok(staffProfile);
            }
            catch (StaffProfilesNotFoundException e)
            {
                return BadRequest(e.Message);
            }
            catch (Exception e)
            {
                return BadRequest(e.Message);
            }
        }

        [HttpGet("search/requests/{patientName?}/{operationType?}/{priority?}/{status?}")]
        [Authorize(Roles = "Doctor")]
        public async Task<IActionResult> SearchRequests(string patientName,string operationType,string priority,string status)
        {
            SeachFilterDto seachFilterDto = new SeachFilterDto
            {
                patientName = patientName.ToLower() == "null" ? null : patientName,
                type = operationType.ToLower() == "null" ? null : operationType,
                priority = priority.ToLower() == "null" ? null : priority,
                status = status.ToLower() == "null" ? null : status,
            };
            try
            {
                var requests = await _staffService.SearchRequestsAsync(seachFilterDto.patientName, seachFilterDto.type,
                    seachFilterDto.priority, seachFilterDto.status,getEmail());

                if (seachFilterDto.status != null)
                {
                    var tableData = new List<object>();

                    for (int i = 0; i < requests.Count; i++)
                    {
                        var operationRequest = requests[i];
                        tableData.Add(new
                        {
                            ID= operationRequest.Id,
                            PatientName = operationRequest.Patient.Person?.FullName.ToString(),
                            OperationType = operationRequest.OperationType.Name.ToString(),
                            Priority = operationRequest.PriorityEnum.ToString(),
                            Status = seachFilterDto.status
                        });
                    }

                    return Ok( tableData );
                }
                else
                {
                    var tableData = new List<object>();

                    for (int i = 0; i < requests.Count; i++)
                    {
                        var operationRequest = requests[i];
                        tableData.Add(new
                        {
                            ID= operationRequest.Id,
                            PatientName = operationRequest.Patient.Person?.FullName.ToString(),
                            OperationType = operationRequest.OperationType.Name.ToString(),
                            Priority = operationRequest.PriorityEnum.ToString(),
                        });
                    }                    
                    return Ok( tableData );

                }
            }
            catch (Exception e)
            {
                return BadRequest($"Error: {e.Message}");
            }
        }

        public string getEmail()
        {
            var claimsIdentity = User.Identity as ClaimsIdentity;
            return claimsIdentity?.FindFirst(ClaimTypes.Email).Value;
        }
    }
}