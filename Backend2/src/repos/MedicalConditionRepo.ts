import {Service, Inject} from 'typedi';
import IMedicalConditionRepo from "../services/IRepos/IMedicalConditionRepo";
import mongoose, {Document, Model} from "mongoose";
import {IMedicalConditionPersistence} from "../dataschema/IMedicalConditionPersistence";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";
import {MedicalConditionMap} from "../mappers/MedicalConditionMap";
import {MedicalConditionId} from "../domain/MedicalCondition/MedicalConditionId";
import {Code} from "../domain/MedicalCondition/code";
import {Designation} from "../domain/MedicalCondition/designation";
import * as domain from "node:domain";
import {NotFoundException} from "../Exceptions/NotFoundException";

@Service()
export default class MedicalConditionRepo implements IMedicalConditionRepo {

    constructor(
        @Inject('medicalConditionSchema') private medicalConditionSchema: Model<IMedicalConditionPersistence & Document>,
    ) {
    }
    public async getMedicalConditionByCode(code: Code): Promise<MedicalCondition> {
        const query = {code: code.value};
        const medicalCondition = await this.medicalConditionSchema.findOne(query);

        return MedicalConditionMap.toDomain(medicalCondition);
    }
    public async searchDesignation(query: string): Promise<MedicalCondition[]> {
        try {
            const conditions = await this.medicalConditionSchema.find(
                { designation: query}).exec();
            console.log("Conditions: ", conditions);


            if (!conditions || conditions.length === 0) {
                return [];
            }

            return conditions.map((condition) => MedicalConditionMap.toDomain(condition));
        } catch (error) {
            console.error("Error while searching for medical conditions:", error);
            throw new Error("Failed to search medical conditions.");
        }
    }
    public async searchCode(query: string): Promise<MedicalCondition[]> {
        try {
            const conditions = await this.medicalConditionSchema.find(
                { code: query}).exec();
            console.log("Conditions: ", conditions);


            if (!conditions || conditions.length === 0) {
                return [];
            }

            return conditions.map((condition) => MedicalConditionMap.toDomain(condition));
        } catch (error) {
            console.error("Error while searching for medical conditions:", error);
            throw new Error("Failed to search medical conditions.");
        }
    }
    public async save(medicalCondition: MedicalCondition, id?: number): Promise<MedicalCondition> {

        if(id === undefined) {
            id = await this.getLastId() + 1;
        }

        const rawMedicalCondition: any = MedicalConditionMap.toPersistence(medicalCondition, id);

        const medicalConditionCreated = await this.medicalConditionSchema.create(rawMedicalCondition);

        return MedicalConditionMap.toDomain(medicalConditionCreated);
    }

    public async getLastId(): Promise<number> {
        const lastElement = await this.medicalConditionSchema.find().sort({domainId: -1}).limit(1);

        if (lastElement.length === 0) {
            return 0;
        }

        return lastElement[0].domainId;
    }

    public async getOne(): Promise<MedicalCondition> {
        const medicalCondition = await this.medicalConditionSchema.findOne();

        return MedicalConditionMap.toDomain(medicalCondition);
    }


  public async exists(medicalConditionId: MedicalCondition | string): Promise<boolean> {

    console.log("Not Implemented Yet");
    return false;
  }

    public async getMedicalConditionByBusinessId(medicalConditionId: string): Promise<any> {

        const objectId = new mongoose.Types.ObjectId(medicalConditionId);

        const medicalCondition = await this.medicalConditionSchema.findOne( { _id: objectId }).exec();

    return MedicalConditionMap.toDomain(medicalCondition);
  }

  public async getByDomainId(id: number): Promise<MedicalCondition> {
    const medicalCondition = await this.medicalConditionSchema.findOne({domainId: id});

    if (!medicalCondition) {
      throw new NotFoundException("Medical Condition not found");
    }

    return MedicalConditionMap.toDomain(medicalCondition);
  }

    public async getMedicalConditionByCode(code: Code): Promise<any> {

        const medicalConditionCode = code.value;

        const medicalCondition = await this.medicalConditionSchema.findOne( { code: medicalConditionCode }).exec();

        console.log(medicalCondition);

        if (!medicalCondition) {
            return undefined;
        }

        return MedicalConditionMap.toDomain(medicalCondition);
    }

    public async getMedicalConditionByDesignation(designation: Designation): Promise<any> {
        const medicalConditionDesignation = designation.value;

        console.log("|" + medicalConditionDesignation + "|");

        // Usando collation para ignorar a diferença de maiúsculas e minúsculas
        const medicalCondition = await this.medicalConditionSchema.findOne({
            designation: medicalConditionDesignation
        }).collation({ locale: 'en', strength: 2 }).exec();  // strength: 2 ignora diferença entre maiúsculas/minúsculas

        if (!medicalCondition) {
            return undefined;
        }

        return MedicalConditionMap.toDomain(medicalCondition);
    }



    public async getAll(): Promise<MedicalCondition[]> {
    const medicalConditions = await this.medicalConditionSchema.find();
    return medicalConditions.map((medicalCondition) => MedicalConditionMap.toDomain(medicalCondition));
  }

  public async update(medicalCondition: MedicalCondition): Promise<MedicalCondition> {
    const privateId = medicalCondition.props._id;
    const domainId = medicalCondition.domainId.value;
    const rawMedicalCondition = MedicalConditionMap.toPersistence(medicalCondition, domainId);

    delete rawMedicalCondition.code;
    delete rawMedicalCondition.domainId;
    delete rawMedicalCondition.designation;

    const updatedMedicalCondition = await this.medicalConditionSchema.findOneAndUpdate(
      {_id: privateId},
      rawMedicalCondition,
      {new: true}
    );

    if (!updatedMedicalCondition) {
      throw new NotFoundException("Medical Condition not found");
    }

    return MedicalConditionMap.toDomain(updatedMedicalCondition);
  }

  public async updateUsingDomainId(medicalCondition: MedicalCondition, ...fieldsToUpdate: string[]): Promise<MedicalCondition> {
    const domainId = medicalCondition.domainId.value;
    const rawMedicalCondition = MedicalConditionMap.toPersistence(medicalCondition, domainId);

    delete rawMedicalCondition.code;
    delete rawMedicalCondition.domainId;
    delete rawMedicalCondition.designation;

    const remainingFields = ['description', 'symptomsList'];

    for (const field of fieldsToUpdate) {
      if (!remainingFields.includes(field)) {
        throw new Error("Invalid field to update");
      }
    }

    Object.keys(rawMedicalCondition).forEach((key) => {
      if (!fieldsToUpdate.includes(key)) {
        delete rawMedicalCondition[key];
      }
    });

    const updatedMedicalCondition = await this.medicalConditionSchema.findOneAndUpdate(
      {domainId: domainId},
      rawMedicalCondition,
      {new: true}
    );

    if (!updatedMedicalCondition) {
      throw new NotFoundException("Medical Condition not found");
    }

    return MedicalConditionMap.toDomain(updatedMedicalCondition);
  }
}
