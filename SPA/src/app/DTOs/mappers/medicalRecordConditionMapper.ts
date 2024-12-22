import {MedicalRecordCondition} from '../../Domain/MedicalRecordCondition';
import {DisplayMedicalRecordConditionDTO} from '../displayDTOs/displayMedicalRecordConditionDTO';

export class MedicalRecordConditionMapper {

  public static domainToDisplayDto(medicalRecordCondition: MedicalRecordCondition): DisplayMedicalRecordConditionDTO {
    return {
      conditionId: medicalRecordCondition.conditionId,
      conditionDesignation: medicalRecordCondition.conditionDesignation,
      medicalRecordId: medicalRecordCondition.medicalRecordId,
      doctorId: medicalRecordCondition.doctorId,
      comment: medicalRecordCondition.comment
    };
  }


}
