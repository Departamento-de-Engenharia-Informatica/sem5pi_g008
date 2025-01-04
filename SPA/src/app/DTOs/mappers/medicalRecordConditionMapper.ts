import {MedicalRecordCondition} from '../../Domain/MedicalRecordCondition/MedicalRecordCondition';
import {DisplayMedicalRecordConditionDTO} from '../displayDTOs/displayMedicalRecordConditionDTO';
import {BackendMedicalRecordConditionDTO} from "../backendDTOs/backendMedicalRecordConditionDTO";


export class MedicalRecordConditionMapper {

    public static domainToDisplayDTO(medicalRecordCondition: MedicalRecordCondition): DisplayMedicalRecordConditionDTO {

        if(!medicalRecordCondition.doctorLicenseNumber) {
            return {
                conditionCode: medicalRecordCondition.conditionCode,
                conditionDesignation: medicalRecordCondition.conditionDesignation,
                doctorName: medicalRecordCondition.doctorName,
                comment: medicalRecordCondition.comment,
              domainId: medicalRecordCondition.domainId
            } as DisplayMedicalRecordConditionDTO;
        }

        return {
            conditionCode: medicalRecordCondition.conditionCode,
            conditionDesignation: medicalRecordCondition.conditionDesignation,
            doctorName: medicalRecordCondition.doctorName,
            doctorLicenseNumber: medicalRecordCondition.doctorLicenseNumber,
            comment: medicalRecordCondition.comment,
          domainId: medicalRecordCondition.domainId

        } as DisplayMedicalRecordConditionDTO;
    }

    public static backendDisplayDTOToDomain(backendDisplayMedicalRecordConditionDTO: BackendMedicalRecordConditionDTO): MedicalRecordCondition {

        if(!backendDisplayMedicalRecordConditionDTO.doctorLicenseNumber) {
            return {
                conditionId: backendDisplayMedicalRecordConditionDTO.conditionId,
                conditionCode: backendDisplayMedicalRecordConditionDTO.conditionCode,
                conditionDesignation: backendDisplayMedicalRecordConditionDTO.conditionDesignation,
                doctorName: backendDisplayMedicalRecordConditionDTO.doctorName,
                comment: backendDisplayMedicalRecordConditionDTO.comment,
                domainId: backendDisplayMedicalRecordConditionDTO.domainId
            } as MedicalRecordCondition;
        }

        return {
            conditionId: backendDisplayMedicalRecordConditionDTO.conditionId,
            conditionCode: backendDisplayMedicalRecordConditionDTO.conditionCode,
            conditionDesignation: backendDisplayMedicalRecordConditionDTO.conditionDesignation,
            doctorName: backendDisplayMedicalRecordConditionDTO.doctorName,
            doctorLicenseNumber: backendDisplayMedicalRecordConditionDTO.doctorLicenseNumber,
            comment: backendDisplayMedicalRecordConditionDTO.comment,
            domainId: backendDisplayMedicalRecordConditionDTO.domainId
        } as MedicalRecordCondition;
    }
}
