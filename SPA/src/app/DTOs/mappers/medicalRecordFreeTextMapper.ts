import {MedicalRecordFreeText} from '../../Domain/MedicalRecordFreeText';
import {DisplayMedicalRecordFreeTextDTO} from '../displayDTOs/displayMedicalRecordFreeTextDTO';
import {BackendMedicalRecordAllergyDTO} from '../backendDTOs/backendMedicalRecordAllergyDTO';
import {MedicalRecordAllergy} from '../../Domain/MedicalRecordAllergy';
import {BackendMedicalRecordFreeTextDTO} from '../backendDTOs/backendMedicalRecordFreeTextDTO';


export class MedicalRecordFreeTextMapper {

  public static domainToDisplayDto(medicalRecordFreeText: MedicalRecordFreeText): DisplayMedicalRecordFreeTextDTO {
    return {
      domainId: medicalRecordFreeText.domainId,
      medicalRecordId:medicalRecordFreeText.medicalRecordId,
      doctorId: medicalRecordFreeText.doctorId,
      comment: medicalRecordFreeText.comment
    };

  }

  public static backendDtoToDomain(medicalRecordFreeText: BackendMedicalRecordFreeTextDTO): MedicalRecordFreeText {
    return {
      domainId: medicalRecordFreeText.domainId,
      medicalRecordId: medicalRecordFreeText.medicalRecordId,
      doctorId: medicalRecordFreeText.doctorId,
      comment: medicalRecordFreeText.comment
    };
  }

}
