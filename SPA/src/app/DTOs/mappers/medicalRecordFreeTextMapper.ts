import {MedicalRecordFreeText} from '../../Domain/MedicalRecordFreeText';
import {DisplayMedicalRecordFreeTextDTO} from '../displayDTOs/displayMedicalRecordFreeTextDTO';


export class MedicalRecordFreeTextMapper {

  public static domainToDisplayDto(medicalRecordFreeText: MedicalRecordFreeText): DisplayMedicalRecordFreeTextDTO {
    return {
      medicalRecordId:medicalRecordFreeText.medicalRecordId,
      doctorId: medicalRecordFreeText.doctorId,
      comment: medicalRecordFreeText.comment
    };

  }

}
