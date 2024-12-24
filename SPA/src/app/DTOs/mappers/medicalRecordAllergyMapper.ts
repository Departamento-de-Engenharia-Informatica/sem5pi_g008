import {MedicalRecordAllergy} from '../../Domain/MedicalRecordAllergy';
import {BackendMedicalRecordAllergyDTO} from '../backendDTOs/backendMedicalRecordAllergyDTO';
import {DisplayMedicalRecordAllergyDTO} from '../displayDTOs/displayMedicalRecordAllergyDTO';

export class MedicalRecordAllergyMapper{

    public static backendDtoToDomain(medicalRecordAllergy: BackendMedicalRecordAllergyDTO): MedicalRecordAllergy {
        return {
          domainId: medicalRecordAllergy.domainId,
          allergy: medicalRecordAllergy.allergy,
          medicalRecordId: medicalRecordAllergy.medicalRecordId,
          doctor: medicalRecordAllergy.doctor,
          comment: medicalRecordAllergy.comment
        };
    }

    public static domainToDisplayDto(medicalRecordAllergy: MedicalRecordAllergy): DisplayMedicalRecordAllergyDTO {
        return {
          domainId: medicalRecordAllergy.domainId,
          allergy: medicalRecordAllergy.allergy,
          medicalRecordId: medicalRecordAllergy.medicalRecordId,
          doctor: medicalRecordAllergy.doctor,
          comment: medicalRecordAllergy.comment
        };
    }

}
