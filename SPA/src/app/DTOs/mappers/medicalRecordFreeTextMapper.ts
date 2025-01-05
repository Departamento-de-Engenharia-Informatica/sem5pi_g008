import {MedicalRecordFreeText} from '../../Domain/MedicalRecordFreeText';
import {DisplayMedicalRecordFreeTextDTO} from '../displayDTOs/displayMedicalRecordFreeTextDTO';
import {BackendMedicalRecordAllergyDTO} from '../backendDTOs/backendMedicalRecordAllergyDTO';
import {BackendMedicalRecordFreeTextDTO} from '../backendDTOs/backendMedicalRecordFreeTextDTO';
import {CreateAllergyDTO} from '../createDTOs/createAllergyDTO';
import {Allergy} from '../../Domain/Allergy/Allergy';
import {CreateFreeTextDTO} from '../createDTOs/createFreeTextDTO';
import {BackendAllergyDTO} from '../backendDTOs/backendAllergyDTO';


export class MedicalRecordFreeTextMapper {

  public static domainToDisplayDto(medicalRecordFreeText: MedicalRecordFreeText): DisplayMedicalRecordFreeTextDTO {
    return {
      domainId: undefined,
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

  public static createDtoToDomain(medicalRecordFreeText: CreateFreeTextDTO): MedicalRecordFreeText {

    return {

      medicalRecordId: medicalRecordFreeText.medicalRecordId,
      doctorId: medicalRecordFreeText.doctorId,
      comment: medicalRecordFreeText.comment
    };
  }

  public static domainToBackendDto(medicalRecordFreeText: MedicalRecordFreeText): BackendMedicalRecordFreeTextDTO {

    let domainId;

    if (medicalRecordFreeText.domainId === undefined) {
      domainId = undefined;
    } else {
      domainId = medicalRecordFreeText.domainId;
    }

    return {
      domainId: domainId,
      medicalRecordId: medicalRecordFreeText.medicalRecordId,
      doctorId: medicalRecordFreeText.doctorId,
      comment: medicalRecordFreeText.comment
    };
  }


}
