import IMedicalRecordAllergyDTO from "../../dto/IMedicalRecordAllergyDTO";
import IMedicalRecordConditionDTO from "../../dto/IMedicalRecordConditionDTO";

export default interface IMedicalRecordService{
    createMedicalRecord(medicalRecordId: string): Promise<void>;
    getAllergies(medicalRecordId: string): Promise<IMedicalRecordAllergyDTO[]>;
    getMedicalRecordConditions(medicalRecordId: string): Promise<any>;
    getMedicalRecordConditionByCode(medicalRecordId: string, conditionCode: string): Promise<any>;
    getMedicalRecordConditionByDesignation(medicalRecordId: string, conditionDesignation: string): Promise<any>;
    updateMedicalConditions(medicalRecordId: any, updatedConditions: any): any;
    getAllMedicalRecordConditions():Promise<IMedicalRecordConditionDTO[]>;

    createFamilyHistory(medicalRecordID: any, familyHistory: any): void;
}
