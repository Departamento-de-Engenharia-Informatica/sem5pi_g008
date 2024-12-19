import {DisplayPatientProfileDTO} from '../displayDTOs/displayPatientProfileDTO';
import {PatientProfile} from '../../Domain/PatientProfile';
import {BackendPatientProfileDTO} from '../backendDTOs/backendPatientProfileDTO';

export class PatientProfileMapper {

  public static domainToDisplayDto(patientProfile: PatientProfile): DisplayPatientProfileDTO {
    return {
      id: patientProfile.id,
      fullName: patientProfile.fullName,
      email: patientProfile.email,
      birthDate: patientProfile.birthDate,
      phoneNumber: patientProfile.phoneNumber
    };
  }

  public static backendDtoToDomain(patientProfile: BackendPatientProfileDTO): PatientProfile {
    return {
      id: patientProfile.id,
      fullName: patientProfile.fullName,
      email: patientProfile.email,
      birthDate: patientProfile.birthDate,
      phoneNumber: patientProfile.phoneNumber,
      firstName: patientProfile.firstName,
      lastName: patientProfile.lastName,
      gender: patientProfile.gender,
      emergencyContact: patientProfile.emergencyContact
    }
  }

}
