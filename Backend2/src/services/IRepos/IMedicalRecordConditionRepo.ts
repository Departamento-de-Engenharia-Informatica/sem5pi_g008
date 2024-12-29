import {Repo} from "../../core/infra/Repo";
import {MedicalRecordCondition} from "../../domain/MedicalRecordCondition/MedicalRecordCondition";

export default interface IMedicalRecordConditionRepo extends Repo<MedicalRecordCondition> {
  save(medicalCondition: MedicalRecordCondition): Promise<MedicalRecordCondition>;
  getMedicalRecordConditionsWithIds(medicalRecordId: string): Promise<any[]>;
  getMedicalRecordConditionByMedicalRecordIdAndConditionId(medicalRecordId: string, medicalConditionCode: string): Promise<any>;
  getAllMedicalRecordConditions(): Promise<MedicalRecordCondition>;
  getMedicalRecordConditionById(id: string): Promise<MedicalRecordCondition>;

}
