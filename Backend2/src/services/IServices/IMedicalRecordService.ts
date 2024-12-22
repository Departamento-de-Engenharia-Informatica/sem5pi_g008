import IMedicalRecordAllergyDTO from "../../dto/IMedicalRecordAllergyDTO";
import IMedicalRecordDTO from "../../dto/IMedicalRecordDTO";

export default interface IMedicalRecordService{
    createMedicalRecord(medicalRecordId: string): Promise<void>;
    getAllergies(medicalRecordId: string): Promise<IMedicalRecordAllergyDTO[]>;
    addFreeText(medicalRecord: IMedicalRecordDTO): Promise<any>;

}
