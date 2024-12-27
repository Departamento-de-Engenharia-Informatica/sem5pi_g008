import IMedicalRecordAllergyDTO from "../../dto/IMedicalRecordAllergyDTO";
import IMedicalRecordDTO from "../../dto/IMedicalRecordDTO";

export default interface IMedicalRecordService{
    createMedicalRecord(medicalRecordId: string): Promise<void>;
    getAllergies(medicalRecordId: string): Promise<IMedicalRecordAllergyDTO[]>;
    addFreeText(medicalRecord: IMedicalRecordDTO): Promise<any>;
    getMedicalRecordConditions(medicalRecordId: string): Promise<any>;
    getMedicalRecordConditionByCode(medicalRecordId: string, conditionCode: string): Promise<any>;
    getMedicalRecordConditionByDesignation(medicalRecordId: string, conditionDesignation: string): Promise<any>;

}
