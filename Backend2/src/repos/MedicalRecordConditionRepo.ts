import {Inject, Service} from "typedi";
import {Document, Model} from "mongoose";
import {IMedicalRecordConditionPersistence} from "../dataschema/IMedicalRecordConditionPersistence";
import {MedicalRecordCondition} from "../domain/MedicalRecordCondition/MedicalRecordCondition";
import IMedicalRecordConditionRepo from "../services/IRepos/IMedicalRecordConditionRepo";
import {MedicalRecordConditionMapper} from "../mappers/MedicalRecordConditionMapper";


@Service()
export default class MedicalRecordConditionRepo implements IMedicalRecordConditionRepo{
  constructor(@Inject('medicalRecordConditionSchema') private medicalRecordConditionSchema: Model<IMedicalRecordConditionPersistence & Document>,) {
  }

  exists(t: MedicalRecordCondition): Promise<boolean> {
    //TODO:Implement exists method
    return Promise.resolve(false);
  }

  public async save(medicalRecordCondition: MedicalRecordCondition): Promise<MedicalRecordCondition> {

    const id = await this.getLastId();

    const rawMedicalRecordCondition: any = MedicalRecordConditionMapper.toPersistence(medicalRecordCondition, id);

    const medicalRecordConditionCreated = await this.medicalRecordConditionSchema.create(rawMedicalRecordCondition);

    return MedicalRecordConditionMapper.toDomain(medicalRecordConditionCreated);
  }


  public async getLastId(): Promise<number> {

    let number = 1;

    const medicalRecord = await this.medicalRecordConditionSchema.find();

    if (medicalRecord.length > 0) {
      number = medicalRecord.length + 1;
    }

    return number;
  }
  
  public async getMedicalRecordConditions(medicalRecordId: string): Promise<MedicalRecordCondition[]> {
    const medicalRecordConditions = await this.medicalRecordConditionSchema.find({medicalRecordId: medicalRecordId});
    return medicalRecordConditions.map(MedicalRecordConditionMapper.toDomain);
  }
}
