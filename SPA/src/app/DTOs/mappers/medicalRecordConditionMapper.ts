import {MedicalRecordCondition} from '../../Domain/MedicalRecordCondition';
import {DisplayMedicalRecordConditionDTO} from '../displayDTOs/displayMedicalRecordConditionDTO';
import {BackendMedicalRecordConditionDTO} from "../backendDTOs/backendMedicalRecordConditionDTO";


export class MedicalRecordConditionMapper {

    public static domainToDisplayDTO(medicalRecordCondition: MedicalRecordCondition): DisplayMedicalRecordConditionDTO {

        if(!medicalRecordCondition.doctorLicenseNumber) {
            return {
                conditionDesignation: medicalRecordCondition.conditionDesignation,
                doctorName: medicalRecordCondition.doctorName,
                comment: medicalRecordCondition.comment
            } as DisplayMedicalRecordConditionDTO;
        }

        return {
            conditionDesignation: medicalRecordCondition.conditionDesignation,
            doctorName: medicalRecordCondition.doctorName,
            doctorLicenseNumber: medicalRecordCondition.doctorLicenseNumber,
            comment: medicalRecordCondition.comment
        } as DisplayMedicalRecordConditionDTO;
    }

    public static backendDisplayDTOToDomain(backendDisplayMedicalRecordConditionDTO: BackendMedicalRecordConditionDTO): MedicalRecordCondition {

        if(!backendDisplayMedicalRecordConditionDTO.doctorLicenseNumber) {
            return {
                conditionDesignation: backendDisplayMedicalRecordConditionDTO.conditionDesignation,
                doctorName: backendDisplayMedicalRecordConditionDTO.doctorName,
                comment: backendDisplayMedicalRecordConditionDTO.comment
            } as MedicalRecordCondition;
        }

        return {
            conditionDesignation: backendDisplayMedicalRecordConditionDTO.conditionDesignation,
            doctorName: backendDisplayMedicalRecordConditionDTO.doctorName,
            doctorLicenseNumber: backendDisplayMedicalRecordConditionDTO.doctorLicenseNumber,
            comment: backendDisplayMedicalRecordConditionDTO.comment
        } as MedicalRecordCondition;
    }
}
